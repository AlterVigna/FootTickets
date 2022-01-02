package it.unipi.dsmt.project.foottickets.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;


// This controller is used for doing tests and experiment new technologies.
@Controller
public class DemoController {


    @Autowired
    DataSource dataSource;


    @RequestMapping("/demo/error")
    @ResponseBody
    public String index(){
        int a=0;
        a=1;
        int b=a/0;


        return "prova";
    }


    @RequestMapping("/prova")
    public ModelAndView index2(ModelMap model) {
        //model.addAttribute("attribute", "redirectWithRedirectPrefix");
        //return new ModelAndView("redirect:/login", model);


        String sqlSelectAllPersons = "SELECT * FROM account";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sqlSelectAllPersons);
             ResultSet rs = ps.executeQuery()) {

            while (rs.next()) {
                // long id = rs.getLong("username");
                String name = rs.getString("username");
                String lastName = rs.getString("password");
                System.out.println( name +lastName);
                // do something with the extracted data...
            }
        } catch (SQLException e) {
            // handle the exception
            e.printStackTrace();
        }


        return new ModelAndView("indexB");
    }


}
