package it.unipi.dsmt.project.foottickets.controller;


import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.security.Principal;
import java.sql.Timestamp;
import java.util.Date;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

@Controller
public class CustomErrorController implements ErrorController {


    @RequestMapping("/error")
    public String handleError(HttpServletRequest request, Model model, Principal principal, Authentication authentication) {

        Exception exception = (Exception) request.getAttribute("javax.servlet.error.exception");
        Object status = request.getAttribute(RequestDispatcher.ERROR_STATUS_CODE);

        if (status != null) {
            Integer statusCode = Integer.valueOf(status.toString());
            if(statusCode == HttpStatus.FORBIDDEN.value()) {
                return ERROR_403;
            }

            if(statusCode == HttpStatus.NOT_FOUND.value()) {
                return ERROR_404;
            }
            else if(statusCode == HttpStatus.INTERNAL_SERVER_ERROR.value()) {
                Timestamp timestamp = new Timestamp(System.currentTimeMillis());
                model.addAttribute("timestamp",timestamp.toString());
                model.addAttribute("path",request.getAttribute(RequestDispatcher.FORWARD_REQUEST_URI));
                model.addAttribute("status",500);
                model.addAttribute("message", "Internal Server Error: "+exception.getMessage());
                model.addAttribute("exception",exception.toString());

                // Convert stack trace to string
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                exception.printStackTrace(pw);
                String sStackTrace = sw.toString(); // stack trace as a string

                model.addAttribute("trace", sStackTrace);
                return ERROR_500;
            }
        }
        // Generic error page
        return "error";
    }

}
