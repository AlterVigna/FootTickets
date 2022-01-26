package it.unipi.dsmt.project.foottickets.controller;


import it.unipi.dsmt.project.foottickets.dto.CreateMapDTO;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import java.security.Principal;
import java.util.Optional;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

@Controller
@RequestMapping("/admin")
public class AdminController {

    @Autowired
    DispatcherInterface dispatcherInterface;

    @Autowired
    @Qualifier("mainAccountService")
    IAccountService accountService;

    @GetMapping("")
    public String admin(HttpServletRequest request, Principal principal){
        Optional<Account> user = accountService.findByUsername(principal.getName());
        request.getSession().setAttribute(KEY_CURRENT_USER,user.get());
        return HOME_ADMIN_PAGE;
    }

    @GetMapping("/createMap")
    public String createMap(HttpServletRequest request, Model model){

        CreateMapDTO mapForm= new CreateMapDTO();
        mapForm.setNumRows(1L);
        mapForm.setNumCols(1L);
        mapForm.setPrice(50L);
        model.addAttribute("mapForm",mapForm);

        return CREATE_MAP_VIEW;
    }

    @PostMapping("/confirmMap")
    public String confirmMap(HttpServletRequest request, @ModelAttribute("map") CreateMapDTO mapForm ){

        System.out.println("Num Rows : "+ mapForm.getNumRows() );
        System.out.println("Num Cols : "+ mapForm.getNumCols() );
        System.out.println("Price per seat : "+ mapForm.getPrice() );
        System.out.println("Selected places : ");
        for (String place:mapForm.getSelectedPlaces()) {
            String[] splitted = place.split("_");
            System.out.println("Row :"+splitted[0]+" Column :"+splitted[1]);
        }


        JSONObject requestJson=null;
        JSONObject responseJson=null;
        boolean outcome=true;

        try {
            requestJson = mapForm.toJSON();
            responseJson = dispatcherInterface.executeClientTask(requestJson);

            if (responseJson!=null){

                int answer = Integer.parseInt(responseJson.getString("answer"));
                if (answer==POSITIVE_ANSWER){
                    String hash=responseJson.getString("hash");
                    dispatcherInterface.getMapState().setHash(hash);
                    dispatcherInterface.getMapState().setNumRows(mapForm.getNumRows());
                    dispatcherInterface.getMapState().setNumCols(mapForm.getNumCols());
                    dispatcherInterface.getMapState().setPrice(mapForm.getPrice());
                    dispatcherInterface.getMapState().setLockedPlaces(mapForm.getSelectedPlaces());
                    //request.getSession().setAttribute(KEY_SELECTED_SEATS,mapForm.getSelectedPlaces());
                }
                else{
                    outcome = false;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            outcome = false;
        }

        return  "redirect:"+HOME_ADMIN+"?mapCreated="+outcome;
    }


    @GetMapping("/viewMap")
    public String confirmMap(HttpServletRequest request){

        return VIEW_MAP_VIEW;
    }





}
