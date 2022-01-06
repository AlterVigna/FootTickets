package it.unipi.dsmt.project.foottickets.controller;


import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import java.security.Principal;
import java.util.Optional;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

@Controller
@RequestMapping("/admin")
public class AdminController {

    @Autowired
    @Qualifier("mainAccountService")
    IAccountService accountService;

    @RequestMapping("")
    public String admin(HttpServletRequest request, Principal principal){
        Optional<Account> user = accountService.findByUsername(principal.getName());
        request.getSession().setAttribute(KEY_CURRENT_USER,user.get());
        return HOME_ADMIN_PAGE;
    }
    @RequestMapping("/createMap")
    public String createMap(HttpServletRequest request){


        return CREATE_MAP_VIEW;
    }





}
