package it.unipi.dsmt.project.foottickets.erlangInterfaces;

import lombok.Getter;
import lombok.Setter;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashSet;
import java.util.Set;

@Setter
@Getter
public class MapState {

    private String hash;

    private Long numRows;
    private Long numCols;
    private Long price;
    private Set<String> lockedPlaces;


    MapState(){
        this.hash="0";
        this.numRows=0L;
        this.numCols=0L;
        this.price=0L;
        this.lockedPlaces=new HashSet<>();
    }


    public JSONObject toJSON() throws JSONException {

        JSONObject json= new JSONObject();
        json.put("hash",this.hash);
        json.put("numRows",this.numRows);
        json.put("numCols",this.numCols);
        json.put("price",this.price);

        JSONArray jsonArray= new JSONArray(this.lockedPlaces);

        json.put("lockedPlaces",jsonArray);

        return json;
    }

}
