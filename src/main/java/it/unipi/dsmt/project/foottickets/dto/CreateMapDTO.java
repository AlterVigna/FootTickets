package it.unipi.dsmt.project.foottickets.dto;

import lombok.Getter;
import lombok.Setter;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashSet;
import java.util.Set;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;


/**
 * This DTO represents the fields form of web page for creating a new seats map.
 */

@Getter
@Setter
public class CreateMapDTO {

    Integer numRows;
    Integer numCols;
    Integer price;
    Set<String> selectedPlaces=new HashSet<>();


    public JSONObject toJSON() throws JSONException {

        JSONObject jo = new JSONObject();
        jo.put("operation",ERL_OP_CODE_CREATE_MAP);
        jo.put("numRows", numRows.intValue());
        jo.put("numCols", numCols.intValue());
        jo.put("price", price.intValue());

        JSONArray jArray = new JSONArray();

        for (String selPlace: selectedPlaces) {
            jArray.put(selPlace);
        }
        jo.put("selectedPlaces",jArray);
        return jo;
    }

}
