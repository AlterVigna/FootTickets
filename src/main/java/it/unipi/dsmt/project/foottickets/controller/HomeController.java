package it.unipi.dsmt.project.foottickets.controller;


import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;


import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;


@Controller
public class HomeController {

    @Autowired
    DispatcherInterface dispatcherInterface;

    @Autowired
    @Qualifier("mainAccountService")
    IAccountService accountService;

    // Used for redirection to correct page in case of authenticated or anonymous user.
    @RequestMapping("/")
    public String index(HttpServletRequest request, Authentication authentication) {

        if (authentication!=null && authentication.isAuthenticated()){
            Account current_user= (Account) request.getSession().getAttribute(KEY_CURRENT_USER);
            if (CODE_ROLE_ADMIN.equals(current_user.getType())){
                return "redirect:"+HOME_ADMIN;
            }
            if (CODE_ROLE_BUYER.equals(current_user.getType())) {
                return "redirect:"+HOME_BUYER;
            }
        }
        return "redirect:"+LOGIN_CALL;
    }

    @PostMapping(value = "/signIn")
    public String signIn(HttpServletRequest request, @ModelAttribute("account") Account account){
        boolean outcome =false;
        account.setAmount(DEFAULT_NEW_AMOUNT);
        // Maybe some further control next..
        if ("".equals(account.getUsername().trim())){
            outcome=false;
        }
        else {
            outcome = accountService.saveNewAccount(account);
        }
        return "redirect:"+LOGIN_CALL+"?registrationOk="+outcome;
    }

    @GetMapping("/login")
    public String login(Model model){
        Account account = new Account();
        account.setType(CODE_ROLE_BUYER);
        model.addAttribute("account",account);
        return LOGIN_PAGE;
    }

}
