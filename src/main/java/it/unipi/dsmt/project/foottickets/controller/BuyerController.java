package it.unipi.dsmt.project.foottickets.controller;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.Transaction;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import it.unipi.dsmt.project.foottickets.service.ITransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import java.security.Principal;
import java.util.List;
import java.util.Optional;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

@Controller
@RequestMapping("/buyer")
public class BuyerController {

    @Autowired
    @Qualifier("mainAccountService")
    IAccountService accountService;

    @Autowired
    @Qualifier("mainTransactionService")
    ITransactionService transactionService;


    @RequestMapping("")
    public String buyer(HttpServletRequest request, Principal principal){
        Optional<Account> user = accountService.findByUsername(principal.getName());
        request.getSession().setAttribute(KEY_CURRENT_USER,user.get());
        return HOME_BUYER_PAGE;
    }

    @RequestMapping("/viewPurchased")
    public String viewPurchased(HttpServletRequest request, Model model){

        Account account= (Account) request.getSession().getAttribute(KEY_CURRENT_USER);
        List<Transaction> listTransactions =transactionService.findLast10TransactionOfAccount(account.getUsername());

        model.addAttribute("listTransactions",listTransactions);

        return VIEW_PURCHASED_PAGE;
    }







}
