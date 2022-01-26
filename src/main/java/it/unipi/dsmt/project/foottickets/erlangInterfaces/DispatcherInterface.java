package it.unipi.dsmt.project.foottickets.erlangInterfaces;

import com.ericsson.otp.erlang.*;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

/**
 * We have just one instance of this class per server.
 * The properties are loaded and configured at the start of the webserver.
 * The idea is that we have one erlang node that acts as communication links between dispatcher erlang node and java webserver.
 * This middleman can have several mail boxes to handle several request at same time.
 * Here a link to see how a java executor works.
 * https://www.baeldung.com/java-executor-service-tutorial
 */

@Component
public class DispatcherInterface {

    private static Random randomGenerator;

    private final String cookie;
    private final List<String> dispatcherNodeNames= new ArrayList<>();
    private final String clientNodeName;
    private static OtpNode clientNode;

    private final String recipientMailBox;
    private static final AtomicInteger counter= new AtomicInteger(0); //shared counter

    private MapState mapState; // Used for caching.

    private DispatcherInterface(@Value("${erlang.cookie}") String cookie,
                                @Value("${erlang.dispatchersListName}") String dispatcherStringListName,
                                @Value("${erlang.clientNodeName}") String clientNodeName,
                                @Value("${erlang.recipientMailBoxName}") String recipientMailBoxName
                                ) throws IOException {
        this.cookie=cookie;
        this.clientNodeName = clientNodeName;
        System.out.println("Application Properties Erlang param read: ");
        System.out.println("erlang.cookie ="+cookie);
        initializeDispatcherNodeNames(dispatcherStringListName);
        this.recipientMailBox=recipientMailBoxName;

        if (cookie!="") {
            this.clientNode = new OtpNode(clientNodeName, cookie);
        }
        else {
            this.clientNode = new OtpNode(clientNodeName);
        }

        randomGenerator=new Random();
        mapState=new MapState();
    }

    private void initializeDispatcherNodeNames(String listOfDispatcher){

        if (listOfDispatcher==null || listOfDispatcher.isEmpty()){
            System.err.println("No dispatcher node is inserted into erlang.dispatchersList application.properties");
            return;
        }
        System.out.println("erlang.dispatchersListName = ");
        for (String dispatcherName: listOfDispatcher.split("&")) {
            dispatcherNodeNames.add(dispatcherName);
            System.out.println(dispatcherName);
        }
    }




    public String getRandomDispatcher(){
        int index = randomGenerator.nextInt(dispatcherNodeNames.size());
        return dispatcherNodeNames.get(index);
    }



    public JSONObject executeClientTask(JSONObject request_body) throws IOException, JSONException, OtpErlangDecodeException, OtpErlangExit {

        int mboxIndex = counter.getAndIncrement();

        OtpMbox mbox = clientNode.createMbox("MailBox_" + mboxIndex);

        System.out.println("Created mailbox " + mbox.getName());

        String op = request_body.getString("operation");

        OtpErlangTuple erlangMsg = null;
        String serverNodeName = "";
        String mailBoxRecipient = "";
        OtpErlangObject msg= null;

        JSONObject response = null;
        serverNodeName = "dispatcher_0@localhost";//getRandomDispatcher();
        mailBoxRecipient = getRecipientMailBox();
        
        switch (op) {

            case JS_OP_CODE_CREATE_MAP:
                response=createMapProtocol(mbox,request_body,mailBoxRecipient,serverNodeName);
                break;

            case JS_OP_CODE_SELECT_PLACE:
                response=selectDeselectProtocol(mbox,request_body,mailBoxRecipient,serverNodeName);
                break;

            case JS_OP_CODE_DESELECT_PLACE:
                response=selectDeselectProtocol(mbox,request_body,mailBoxRecipient,serverNodeName);
                break;

            case JS_OP_CODE_SHOW_MAP:
                response=showMapProtocol(mbox,request_body,mailBoxRecipient,serverNodeName);
                break;

            default:
                throw new IllegalStateException("Unexpected value: " + op);
        }
        return response;
    }



    public JSONObject createMapProtocol(OtpMbox mbox, JSONObject request_body, String mailBoxRecipient, String serverNodeName) throws JSONException, OtpErlangDecodeException, OtpErlangExit {

        OtpErlangTuple erlangMsg = DispatcherConversionUtilities.createMapErlangRequest(mbox.self(), request_body);

        mbox.send(mailBoxRecipient,serverNodeName, erlangMsg);
        System.out.println("Request sent .. Wait for an answer.");

        //blocking receive operation
        OtpErlangObject msg = mbox.receive();
        //Here put the timeout and repeting of request.
        JSONObject response = DispatcherConversionUtilities.answerCreateMapFromErlang((OtpErlangTuple) msg);

        System.out.println("Answer received " + response.toString());

        return response;
    }



    public JSONObject selectDeselectProtocol( OtpMbox mbox, JSONObject request_body, String mailBoxRecipient, String serverNodeName) throws JSONException, OtpErlangDecodeException, OtpErlangExit {

        OtpErlangTuple erlangMsg = DispatcherConversionUtilities.selectDeselectErlangRequest(mbox.self(), request_body);
        mbox.send(mailBoxRecipient,serverNodeName, erlangMsg);
        System.out.println("Request sent .. Wait for an answer.");
        
        //blocking receive operation
        OtpErlangObject msg = mbox.receive();
        JSONObject response = DispatcherConversionUtilities.answerSelectDeselectFromErlang((OtpErlangTuple) msg);
        System.out.println("Answer received " + response.toString());
        
        return response;
    }
    
    
    
    public JSONObject showMapProtocol( OtpMbox mbox, JSONObject request_body, String mailBoxRecipient, String serverNodeName) throws JSONException, OtpErlangDecodeException, OtpErlangExit {

        String hash="";
        try{
            hash=request_body.getString("hash");
        }
        catch(Exception ex){
            System.out.println("No hash present. Maybe first time.");
        }

        OtpErlangTuple erlangMsg = DispatcherConversionUtilities.showMapErlangRequest(mbox.self(), hash);
        mbox.send(mailBoxRecipient, serverNodeName, erlangMsg);

        System.out.println("Request sent by " + clientNodeName + " the following tuple: " + erlangMsg.toString());

        //blocking receive operation
        OtpErlangObject msg = mbox.receive();
        JSONObject response = DispatcherConversionUtilities.answerShowMapFromErlang((OtpErlangTuple) msg);

        //Maybe here consider the possibility to store an hash of the entire map.
        System.out.println("Map returned. --");
        
        return response;
    }
    
    
    
    
    
    
    public String getCookie() {
        return cookie;
    }

    public List<String> getDispatcherNodeNames() {
        return dispatcherNodeNames;
    }

    public OtpNode getClientNode() {
        return clientNode;
    }

    public String getRecipientMailBox() {
        return recipientMailBox;
    }

    public String getClientNodeName() {
        return clientNodeName;
    }

    public MapState getMapState() {
        return mapState;
    }

    public void setMapState(MapState mapState) {
        this.mapState = mapState;
    }

}
