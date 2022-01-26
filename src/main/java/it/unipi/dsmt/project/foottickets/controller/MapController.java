package it.unipi.dsmt.project.foottickets.controller;

import it.unipi.dsmt.project.foottickets.dto.MapDTO;
import it.unipi.dsmt.project.foottickets.dto.SeatInfo;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.model.Account;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
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


    @Autowired
    private DispatcherInterface dispatcherInterface;

    // This request is idempotent
    // Return the JSON of the actual seats map
    @GetMapping("/map")
    public ResponseEntity getCompleteMap(HttpServletRequest request){

        MapDTO map= new MapDTO();

        JSONObject requestJson=null;
        JSONObject responseJson=null;
        try{
            requestJson= new JSONObject();
            requestJson.put("operation",ERL_OP_CODE_SHOW_MAP);

            if (request.getSession()!=null){
                requestJson.put("hash",dispatcherInterface.getMapState().getHash());
            }


            responseJson=dispatcherInterface.executeClientTask(requestJson);
            int answer = Integer.parseInt(responseJson.getString("answer"));

            switch (answer){
                case POSITIVE_ANSWER:

                    String hash=responseJson.getString("hash");

                    map.setNumRows(responseJson.getLong("numRows"));
                    map.setNumCols(responseJson.getLong("numCols"));
                    map.setPrice(responseJson.getLong("price"));

                    JSONArray jsonArray = responseJson.getJSONArray("lockedPlaces");
                    for (int i=0;i<jsonArray.length();i++){
                        String lockedPlace = jsonArray.getString(i);
                        map.getLockedPlaces().add(lockedPlace);
                    }

                    dispatcherInterface.getMapState().setHash(hash);
                    dispatcherInterface.getMapState().setNumRows(map.getNumRows());
                    dispatcherInterface.getMapState().setNumCols(map.getNumCols());
                    dispatcherInterface.getMapState().setPrice(map.getPrice());
                    dispatcherInterface.getMapState().setLockedPlaces(map.getLockedPlaces());

                    addAdditionalSelectedPlaces(map,request);

                    break;

                case HASH_MATCHES:

                    map.setNumRows(dispatcherInterface.getMapState().getNumRows());
                    map.setNumCols(dispatcherInterface.getMapState().getNumCols());
                    map.setPrice(dispatcherInterface.getMapState().getPrice());
                    map.setLockedPlaces(dispatcherInterface.getMapState().getLockedPlaces());
                    addAdditionalSelectedPlaces(map,request);
                    break;

                case NEGATIVE_ANSWER:
                    //todo
                    break;

                default:
                    System.out.println("Should never enter here!");
                    break;
            }
        }
        catch (Exception ex){
            ex.printStackTrace();
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

        JSONObject requestJson=null;
        JSONObject responseJson=null;
        try{

            requestJson= new JSONObject();
            requestJson.put("operation",seatInfo.get().getOperation());
            requestJson.put("placeSelected",seatInfo.get().getPlaceSelected());

            responseJson = dispatcherInterface.executeClientTask(requestJson);

            int answer = Integer.parseInt(responseJson.getString("answer"));

            String hash=responseJson.getString("hash");

            map.setNumRows(dispatcherInterface.getMapState().getNumRows());
            map.setNumCols(dispatcherInterface.getMapState().getNumCols());
            map.setPrice(dispatcherInterface.getMapState().getPrice());

            JSONArray jsonArray = responseJson.getJSONArray("lockedPlaces");
            for (int i=0;i<jsonArray.length();i++){
                String lockedPlace = jsonArray.getString(i);
                map.getLockedPlaces().add(lockedPlace);
            }
            dispatcherInterface.getMapState().setHash(hash);
            dispatcherInterface.getMapState().setLockedPlaces(map.getLockedPlaces());

            Set<String> currentSelectedSeats=null;
            if (request.getSession()!=null && request.getSession().getAttribute(KEY_SELECTED_SEATS)!=null){
                currentSelectedSeats=(Set<String>) request.getSession().getAttribute(KEY_SELECTED_SEATS);
            }
            else {
                currentSelectedSeats= new HashSet<>();
            }

            if (answer==POSITIVE_ANSWER){
                map.setAnswer(POSITIVE_ANSWER);

                if (JS_OP_CODE_SELECT_PLACE.equals(seatInfo.get().getOperation())){

                    currentSelectedSeats.add(seatInfo.get().getPlaceSelected());

                    // The answer to web page is all the seats selected during the session activity.
                    for (String seat:currentSelectedSeats) {
                        map.getCurrentSelectedPlaces().add(seat);
                    }
                }

                if (JS_OP_CODE_DESELECT_PLACE.equals(seatInfo.get().getOperation())){
                    currentSelectedSeats.remove(seatInfo.get().getPlaceSelected());
                    for (String seat:currentSelectedSeats) {
                        map.getCurrentSelectedPlaces().add(seat);
                    }
                }

                // Before answer positive I update the session with current values.
                request.getSession().setAttribute(KEY_SELECTED_SEATS,currentSelectedSeats);
            }
            else {
                // Handled in HTML like a popup msg.
                map.setAnswer(NEGATIVE_ANSWER);
            }
        }
        catch (Exception ex){
            ex.printStackTrace();

            // Here finish to implement negative answers.
        }

        map.setResponseCode(HttpStatus.OK.value());
        return ResponseEntity.ok(map);
    }

    // If it realizes that is a logged user who made the request, it adds the places selected by him stored in the session.
    private void addAdditionalSelectedPlaces(MapDTO map, HttpServletRequest request){

        Set<String> listBuyerSelectedSeats=null;
        if (request.getSession()!=null && request.getSession().getAttribute(KEY_SELECTED_SEATS)!=null){
            listBuyerSelectedSeats=(Set<String>) request.getSession().getAttribute(KEY_SELECTED_SEATS);
            for (String seat:listBuyerSelectedSeats) {
                map.getCurrentSelectedPlaces().add(seat);
            }
        }

    }



}
