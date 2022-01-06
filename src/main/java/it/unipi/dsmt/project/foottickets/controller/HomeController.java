package it.unipi.dsmt.project.foottickets.controller;


import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.security.Principal;
import java.util.Optional;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;


@Controller
public class HomeController {

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
        boolean esit =false;
        account.setAmount(DEFAULT_NEW_AMOUNT);
        // Maybe some further control next..
        if ("".equals(account.getUsername().trim())){
            esit=false;
        }
        else {
            esit = accountService.saveAccount(account);
        }
        return "redirect:"+LOGIN_CALL+"?registrationOk="+esit;
    }

    @RequestMapping("/login")
    public String login(Model model){
        Account account = new Account();
        account.setType(CODE_ROLE_BUYER);
        model.addAttribute("account",account);
        return LOGIN_PAGE;
    }

}
