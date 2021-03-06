package it.unipi.dsmt.project.foottickets.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.Setter;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.ERL_OP_CODE_CREATE_MAP;


/**
 * This DTO represents the content of the answer of a Map request from client.
 * answer = 1 indicates if the request is correctly handled, 0 otherwise.
 * numRos indicates the number of rows of a created map.
 * numCols indicates the number of cols of a created map.
 * price represents the cost of single seat in the map.
 * lockedPlaces is a set of seat that are previously reserved in the map,
 * currentSelectedPlaces is a subset of lockedPlaces and represents the seats correctly reserved by the current user.
 *
 * responseCode indicates the HTTP status of the request for further implementations.
 * messageDescription indicates, in case of errors, the description of the error to make an easyer debug.
 */

@Getter
@Setter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class MapDTO implements Serializable {


    Integer answer;
    Long numRows;
    Long numCols;
    Long price;
    Set<String> lockedPlaces=new HashSet<>(); // All locked placed (by other)
    Set<String> currentSelectedPlaces=new HashSet<>(); // All places selected by the user who ask to buy

    int responseCode;
    String messageDescription;


    public JSONObject toJSON() throws JSONException {

        JSONObject jo = new JSONObject();
        jo.put("answer",answer);
        if (numRows.intValue()!=0){
            jo.put("numRows", numRows.intValue());
        }
        if (numCols.intValue()!=0){
            jo.put("numCols", numCols.intValue());
        }
        if (price.intValue()!=0){
            jo.put("price", price.intValue());
        }

        JSONArray jArray = new JSONArray();

        for (String selPlace: lockedPlaces) {
            jArray.put(selPlace);
        }
        jo.put("lockedPlaces",jArray);

        JSONArray jArray2 = new JSONArray();

        for (String selPlace: currentSelectedPlaces) {
            jArray2.put(selPlace);
        }
        jo.put("currentSelectedPlaces",jArray2);

        jo.put("responseCode", responseCode);
        jo.put("messageDescription", messageDescription);

        return jo;
    }


}
