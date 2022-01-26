package it.unipi.dsmt.project.foottickets.dto;

import lombok.Getter;
import lombok.Setter;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;

/**
 * This DTO represent the information from the web client.
 */
@Getter
@Setter
public class SeatInfo implements Serializable {

    public String operation;
    public String placeSelected;

    public JSONObject toJSON() throws JSONException {
        JSONObject jo = new JSONObject();
        jo.put("operation", operation);
        jo.put("placeSelected", placeSelected);
        return jo;
    }
}
