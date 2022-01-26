package it.unipi.dsmt.project.foottickets.erlangInterfaces;

import com.ericsson.otp.erlang.*;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashSet;
import java.util.Set;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

public class DispatcherConversionUtilities {


// Here all the JSON converted into ErlangTuples.

    /**
     * This method receives as input a JSON request and transform it to an Erlang Tuple to inform the dispatcher about
     * the request of creation of new map.
     * JSON Input:
     *     {
     *      "From": name_server,
     *      "op":"create",
     *      "num_cols": N,
     *      "num_rows": M,
     *      "price" : P,
     *      "list_locked":[0_0,0_1]
     *      };
     *
     *  Erlang tuple Output:
     *      {From,create,N,M,P,Map}
     *
     * @param self the PID of Erlang interlocutor on Java Server side.
     * @param obj  the content of the request.
     * @return the Tuple representing the JSONObject
     * @throws JSONException
     */
    public static OtpErlangTuple createMapErlangRequest(OtpErlangPid self,JSONObject obj) throws JSONException {

        if (!obj.getString("operation").equals(JS_OP_CODE_CREATE_MAP)){
            throw new JSONException("Operation Code does not match.");
        }

        OtpErlangAtom opcode = new OtpErlangAtom(ERL_OP_CODE_CREATE_MAP);

        Integer numRows=obj.getInt("numRows");
        Integer numCols=obj.getInt("numCols");
        Integer price=obj.getInt("price");

        if (numRows==null || numCols==null || price==null){
            throw new JSONException("Not all the fields are correctly populated.");
        }

        OtpErlangLong erlNumRows = new OtpErlangLong(numRows);
        OtpErlangLong erlNumCols = new OtpErlangLong(numCols);
        OtpErlangLong erlPrice = new OtpErlangLong(price);

        JSONArray jArray = obj.getJSONArray("selectedPlaces");
        Set<String> lockedPlaces = new HashSet<>();
        for(int i = 0; i < jArray.length(); i++){
            lockedPlaces.add(jArray.getString(i));
        }

        OtpErlangMap erlMap = createNewErlangMap(numRows, numCols, lockedPlaces);

        OtpErlangTuple returnTuple = new OtpErlangTuple(new OtpErlangObject[]{self, opcode, erlNumRows,erlNumCols,erlPrice,erlMap});

        return returnTuple;
    }

    /**
     * This method receives as input a JSON request and transform it to an Erlang Tuple to inform the dispatcher about
     * the request of selection/deselection of a place inside the map.
     *
     * JSON Input:
     *     {
     *      "From": name_server,
     *      "op":"select/deselect",
     *      "seat": "R_C",
     *      };
     *
     * Erlang tuple Output:
     *      {From,select/deselect,"R_C"}
     *
     * @param self the PID of Erlang interlocutor on Java Server side.
     * @param obj  the content of the request.
     * @return the Tuple representing the JSONObject
     * @throws JSONException
     */

public static OtpErlangTuple selectDeselectErlangRequest(OtpErlangPid self,JSONObject obj) throws JSONException {

    if (!obj.getString("operation").equals(JS_OP_CODE_SELECT_PLACE) && !obj.getString("operation").equals(JS_OP_CODE_DESELECT_PLACE)){
        throw new JSONException("Operation Code does not match.");
    }

    OtpErlangAtom opcode=null;

    String op=obj.getString("operation");
    if (op.equals(JS_OP_CODE_SELECT_PLACE)){
        opcode= new OtpErlangAtom(ERL_OP_CODE_SELECT_PLACE);
    }
    if (op.equals(JS_OP_CODE_DESELECT_PLACE)){
        opcode= new OtpErlangAtom(ERL_OP_CODE_DESELECT_PLACE);
    }

    String placeSel=obj.getString("placeSelected");
    if (placeSel==null){
        throw new JSONException("No place selected/deselected");
    }

    OtpErlangString erlPlaceString= new OtpErlangString(placeSel);
    //OtpErlangTuple keyTuple = new OtpErlangTuple(erlPlaceString);
    //OtpErlangString place= new OtpErlangString()

    OtpErlangTuple returnTuple = new OtpErlangTuple(new OtpErlangObject[]{self, opcode, erlPlaceString});

    return returnTuple;

}

    /**
     * This method receives as input a JSON request and transform it to an Erlang Tuple to inform the dispatcher about
     * the request of seeing the entire map.
     *
     *  JSON Input:
     *     {
     *      "From": name_server,
     *      "op":"map"
     *      };
     *
     *  Erlang tuple Output:
     *     {From,map}
     *
     * @param self
     * @return
     * @throws JSONException
     */
public static OtpErlangTuple showMapErlangRequest(OtpErlangPid self,String localHash) throws JSONException {
    OtpErlangAtom opcode = new OtpErlangAtom(ERL_OP_CODE_SHOW_MAP);

    OtpErlangLong hash = new OtpErlangLong(Long.parseLong(localHash));
    OtpErlangTuple returnTuple = new OtpErlangTuple(new OtpErlangObject[]{self, opcode,hash});

    return returnTuple;
}


// Here the Erlang Tuple converted into JSONObject.

// hash,answer (0/1), message
public static JSONObject answerCreateMapFromErlang (OtpErlangTuple msgReply){

    String answer="";
    Long hash=0L;
    String msg="";

    OtpErlangAtom ErlAnswer = (OtpErlangAtom) msgReply.elementAt(0);
    answer=ErlAnswer.atomValue();

    OtpErlangLong ErlHash = (OtpErlangLong) msgReply.elementAt(1);
    hash=ErlHash.longValue();

    OtpErlangString ErlMsg = (OtpErlangString) msgReply.elementAt(2);
    msg=ErlMsg.stringValue();


    if (ERL_POS_ANSWER.equals(answer)){
        answer=POSITIVE_ANSWER+"";
    }
    else {
        answer=NEGATIVE_ANSWER+"";
    }

    JSONObject json= new JSONObject();
    try {
        json.put("answer",answer);
        json.put("hash",hash);
        json.put("msg",msg);

    } catch (JSONException e) {
        e.printStackTrace();
    }

    return json;
}
//server:loop2({N,M,P,FullMap});
public static JSONObject answerSelectDeselectFromErlang (OtpErlangTuple msgReply){

    OtpErlangAtom ErlAnswer = (OtpErlangAtom) msgReply.elementAt(0);
    String answer = ErlAnswer.atomValue();

    OtpErlangString ErlMsg = (OtpErlangString) msgReply.elementAt(2);
    String msg = ErlMsg.stringValue();

    MapState mapState=populateErlangMapStateSEL_DES(msgReply);

    if (ERL_POS_ANSWER.equals(answer)){
        answer=POSITIVE_ANSWER+"";
    }
    else {
        answer=NEGATIVE_ANSWER+"";
    }

    JSONObject json =new JSONObject();
    try {
        json = mapState.toJSON();

        json.put("answer",answer);
        json.put("msg",msg);

    } catch (JSONException e) {
        e.printStackTrace();
    }
    return json;
}



public static JSONObject answerShowMapFromErlang (OtpErlangTuple msgReply){

    OtpErlangAtom ErlAnswer = (OtpErlangAtom) msgReply.elementAt(0);
    String answer = ErlAnswer.atomValue();


    JSONObject json =new JSONObject();

    if (ERL_POS_ANSWER.equals(answer)){
        answer=POSITIVE_ANSWER+"";

        /*OtpErlangString ErlMsg = (OtpErlangString) msgReply.elementAt(2);
        String msg = ErlMsg.stringValue();*/

        MapState mapState=populateErlangMapStateSHOWMAP(msgReply);

        try {
            json = mapState.toJSON();

            json.put("answer",answer);
            json.put("msg","");

        } catch (JSONException e) {
            e.printStackTrace();
        }


    }
    else if (ERL_HASH_MATCHES.equals(answer)){
        answer=HASH_MATCHES+"";

        try {
            json.put("answer",answer);

        } catch (JSONException e) {
            e.printStackTrace();
        }

    }
    else {
        answer=NEGATIVE_ANSWER+"";
        OtpErlangString ErlMsg = (OtpErlangString) msgReply.elementAt(2);
        String msg = ErlMsg.stringValue();

        try {
            json.put("answer",answer);
            json.put("msg",msg);
            // TODO
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    return json;
}





private static Set<String> convertMapReceived(OtpErlangMap ErlMap){
    Set<String> lockedPlaces = new HashSet<>();
    if (ErlMap!=null && ErlMap.keys().length>0){
        for (OtpErlangObject erlPlace:ErlMap.keys()) {

            OtpErlangTuple tupleValue= (OtpErlangTuple) ErlMap.get(erlPlace);
            OtpErlangAtom valueAtom = (OtpErlangAtom) tupleValue.elementAt(0);
            String value=valueAtom.atomValue();
            if (ERL_PLACE_BUSY.equals(value)){
                OtpErlangTuple tupleValueKey= (OtpErlangTuple) erlPlace;
                OtpErlangString stringKey = (OtpErlangString) tupleValueKey.elementAt(0);
                lockedPlaces.add(stringKey.stringValue());
            }
        }
    }
    return lockedPlaces;
}

private static OtpErlangMap createNewErlangMap(int totRows, int totCols,Set<String>lockedPlaces){

    int positions=totRows*totCols;
    OtpErlangObject [] keys= new OtpErlangObject[positions];
    OtpErlangObject [] values= new OtpErlangObject[positions];

    for(int i=0; i<totRows;i++){
        for(int j=0;j<totCols;j++){
            keys[i*totCols+j]=new OtpErlangTuple(new OtpErlangString(i+"_"+j));
            String status=ERL_PLACE_FREE;
            if (lockedPlaces.contains(i+"_"+j)){
                status=ERL_PLACE_BUSY;
            }
            values[i*totCols+j]=new OtpErlangTuple(new OtpErlangAtom(status));
        }
    }
    OtpErlangMap newMap= new OtpErlangMap(keys,values);
    System.out.println(newMap.toString());

    return newMap;
}



private static MapState populateErlangMapStateSHOWMAP(OtpErlangTuple msgReply){

    MapState newMap= new MapState();


    OtpErlangLong ErlHash = (OtpErlangLong) msgReply.elementAt(1);
    Long hash = ErlHash.longValue();


    OtpErlangLong ErlNumRows= (OtpErlangLong) msgReply.elementAt(2);
    long numRows = ErlNumRows.longValue();

    OtpErlangLong ErlNumCols= (OtpErlangLong) msgReply.elementAt(3);
    long numCols = ErlNumCols.longValue();

    OtpErlangLong ErlNumPrice= (OtpErlangLong) msgReply.elementAt(4);
    long price = ErlNumPrice.longValue();
    
    Set<String> lockedPlaces= new HashSet<>();
    OtpErlangMap ErlMap =(OtpErlangMap) msgReply.elementAt(5);
    lockedPlaces.addAll(convertMapReceived(ErlMap));

    newMap.setHash(hash+"");
    newMap.setNumRows(numRows);
    newMap.setNumCols(numCols);
    newMap.setPrice(price);
    newMap.setLockedPlaces(lockedPlaces);

    return newMap;
}


    private static MapState populateErlangMapStateSEL_DES(OtpErlangTuple msgReply){

        MapState newMap= new MapState();

        OtpErlangLong ErlHash = (OtpErlangLong) msgReply.elementAt(1);
        Long hash = ErlHash.longValue();

        Set<String> lockedPlaces= new HashSet<>();
        OtpErlangMap ErlMap =(OtpErlangMap) msgReply.elementAt(3);
        lockedPlaces.addAll(convertMapReceived(ErlMap));

        newMap.setHash(hash+"");
        newMap.setLockedPlaces(lockedPlaces);

        return newMap;
    }


}
