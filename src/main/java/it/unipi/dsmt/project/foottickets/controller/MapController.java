package it.unipi.dsmt.project.foottickets.controller;

import it.unipi.dsmt.project.foottickets.dto.MapDTO;
import it.unipi.dsmt.project.foottickets.dto.SeatInfo;
import it.unipi.dsmt.project.foottickets.model.Account;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.*;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

// This path is voluntarily not protected by spring security because we could use  the /rest/map
// from outside to see the current status of the map.

@RestController
@RequestMapping("/rest")
public class MapController {


    // This request is idempotent
    // Return the JSON of the actual seats map
    @GetMapping("/map")
    public ResponseEntity getCompleteMap(HttpServletRequest request){


        // TODO IMPLEMENT A CALL TO DISPATCHER

        // Evaluate the fact of using a cache of the current map to avoid sending all time requests to dispatcers.
        MapDTO map= new MapDTO();
        map.setNumRows(1);
        map.setNumCols(15);
        map.setPrice(50);
        map.getLockedPlaces().add("0_1");
        map.getLockedPlaces().add("0_2");
        map.getLockedPlaces().add("0_3");
        // This is a sort of answer we expect from dispatcher.

        // If it realizes that is a logged user who made the request, it adds the places stored in the session.
        Set<String> listBuyerSelectedSeats=null;
        if (request.getSession()!=null && request.getSession().getAttribute(KEY_SELECTED_SEATS)!=null){
            listBuyerSelectedSeats=(Set<String>) request.getSession().getAttribute(KEY_SELECTED_SEATS);
            for (String seat:listBuyerSelectedSeats) {
                map.getCurrentSelectedPlaces().add(seat);
            }
            // From dispatcher this is not fixed..
            request.getSession().setAttribute(KEY_SEAT_COST,50);
        }
        return ResponseEntity.ok(map);
    }


    // This Rest call can be done just from authenticated users, in case of anonymous user the request will return a 401 not authorized.
    // Authentication here is checked manually.
    // It tries to contact dispatcher erlang node to reserve or free a seat.
    @PostMapping(value = "/reserveSeat" , produces = "application/json")
    public ResponseEntity reserveAPlace(HttpServletRequest request, Authentication authentication,Optional<SeatInfo> seatInfo){

        MapDTO map= new MapDTO();

        // Preliminary checks.
        // Check if user is logged.
        if (authentication==null || !authentication.isAuthenticated() ){
            map.setAnswer(NEGATIVE_ANSWER);
            map.setResponseCode(HttpStatus.UNAUTHORIZED.value());
            map.setMessageDescription("Authentication is needed.");
            return new ResponseEntity<MapDTO>(map, HttpStatus.UNAUTHORIZED);
        }
        // Check if operation and the seat to do operation are specified.
        if (!seatInfo.isPresent() || seatInfo.get().getOperation()==null || seatInfo.get().getOperation().trim()==""
                || seatInfo.get().getPlaceSelected()==null || seatInfo.get().getPlaceSelected()==""){

                map.setAnswer(NEGATIVE_ANSWER);
                map.setResponseCode(HttpStatus.BAD_REQUEST.value());
                map.setMessageDescription("Parameter are not correct.");
                return new ResponseEntity<MapDTO>(map, HttpStatus.BAD_REQUEST);
        }

        // Here starts the logic.

        Account account=(Account) request.getSession().getAttribute(KEY_CURRENT_USER);



        // TODO IMPLEMENT A CALL TO DISPATCHER

        // if positive answer
        map.setAnswer(POSITIVE_ANSWER);
        map.setNumRows(1);
        map.setNumCols(15);
        map.setPrice(50);
        map.getLockedPlaces().add("0_1");
        map.getLockedPlaces().add("0_2");
        map.getLockedPlaces().add("0_3");


        // From dispatcher this is not fixed.
        request.getSession().setAttribute(KEY_SEAT_COST,50);

        Set<String> currentSelectedSeats=null;
        if (request.getSession()!=null && request.getSession().getAttribute(KEY_SELECTED_SEATS)!=null){
            currentSelectedSeats=(Set<String>) request.getSession().getAttribute(KEY_SELECTED_SEATS);
        }
        else {
            currentSelectedSeats= new HashSet<>();
        }

        if ("select".equals(seatInfo.get().getOperation())){

            currentSelectedSeats.add(seatInfo.get().getPlaceSelected());

            // The answer to web page is all the seats selected during the session activity.
            for (String seat:currentSelectedSeats) {
                map.getCurrentSelectedPlaces().add(seat);
            }
        }

        if ("deselect".equals(seatInfo.get().getOperation())){
            currentSelectedSeats.remove(seatInfo.get().getPlaceSelected());
            for (String seat:currentSelectedSeats) {
                map.getCurrentSelectedPlaces().add(seat);
            }
        }

        // Before answer positive I update the session with current values.
        request.getSession().setAttribute(KEY_SELECTED_SEATS,currentSelectedSeats);

        map.setAnswer(POSITIVE_ANSWER);
        map.setResponseCode(HttpStatus.OK.value());

        return ResponseEntity.ok(map);

    }






}
