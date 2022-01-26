package it.unipi.dsmt.project.foottickets.controller;

import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.Transaction;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import it.unipi.dsmt.project.foottickets.service.ITransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import java.security.Principal;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

@Controller
@RequestMapping("/buyer")
public class BuyerController {

    @Autowired
    DispatcherInterface dispatcherInterface;

    @Autowired
    @Qualifier("mainAccountService")
    IAccountService accountService;

    @Autowired
    @Qualifier("mainTransactionService")
    ITransactionService transactionService;


    @GetMapping("")
    public String buyer(HttpServletRequest request, Principal principal){
        Optional<Account> user = accountService.findByUsername(principal.getName());
        request.getSession().setAttribute(KEY_CURRENT_USER,user.get());
        return HOME_BUYER_PAGE;
    }

    @GetMapping("/viewPurchased")
    public String viewPurchased(HttpServletRequest request, Model model){

        Account account= (Account) request.getSession().getAttribute(KEY_CURRENT_USER);
        List<Transaction> listTransactions =transactionService.findLast10TransactionOfAccount(account.getUsername());

        model.addAttribute("listTransactions",listTransactions);

        return VIEW_PURCHASED_PAGE;
    }


    @GetMapping("/buyTicket")
    public String buyTicketMap(HttpServletRequest request){

        return BUY_TICKET_VIEW;
    }


    @PostMapping("/executeBuy")
    public String executeBuy(HttpServletRequest request){

        // Check if operation is possible

        Set<String> currentSelectedSeats = (Set<String>) request.getSession().getAttribute(KEY_SELECTED_SEATS);
        if (currentSelectedSeats==null || currentSelectedSeats.isEmpty()){
            return  "redirect:"+HOME_BUYER+"/buyTicket?errorNoSelectedSeat="+true;
        }

        // I read again from db to have a currect view of amount of logged user.
        Account account= (Account) request.getSession().getAttribute(KEY_CURRENT_USER);
        Long seatPrice = dispatcherInterface.getMapState().getPrice();
                //(Integer) request.getSession().getAttribute(KEY_SEAT_COST);

        Optional<Account> updatedAccount = accountService.findByUsername(account.getUsername());
        if (updatedAccount.isPresent()){
            account=updatedAccount.get();
        }

        int numPlaces = currentSelectedSeats.size();

        if (numPlaces*seatPrice>account.getAmount()){
            return  "redirect:"+HOME_BUYER+"/buyTicket?errorNoMoney="+true;
        }


        String location="";
        for (String seat: currentSelectedSeats) {
            location+=seat+"; ";
        }

        Transaction transaction= new Transaction();

        transaction.setNumSeats(numPlaces);
        transaction.setLocation(location);
        transaction.setPrice(numPlaces*seatPrice.doubleValue());
        transaction.setTimestamp(null); // This will be insert automatically by db.
        transaction.setAccount(account);

        // This method is a transactional so all the checks are repeated inside.
        try {
            transactionService.saveTransactionAndUpdateAccount(transaction,account);
        } catch (Exception e) {
            return  "redirect:"+HOME_BUYER+"/buyTicket?errorDuringSave="+true;
        }

        request.getSession().removeAttribute(KEY_SELECTED_SEATS);
        return  "redirect:"+HOME_BUYER+"?purchasedTicket=true";
    }






}
