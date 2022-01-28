package it.unipi.dsmt.project.foottickets.erlangInterfaces;

import com.ericsson.otp.erlang.*;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.*;
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
    private final Set<String> dispatcherNodeNames= new HashSet<>();
    private final Set<String> dispatcherNodesAvailables=new HashSet<>();

    private final String clientNodeName;
    private static OtpNode clientNode;

    private final String recipientMailBox;
    private static final AtomicInteger counter= new AtomicInteger(0); //shared counter
    private final  Long MAX_ATTEMPTS;
    private final  Long MAX_TIMEOUT;

    private MapState mapState; // Used for caching.

    private DispatcherInterface(@Value("${erlang.cookie}") String cookie,
                                @Value("${erlang.dispatchersListName}") String dispatcherStringListName,
                                @Value("${erlang.clientNodeName}") String clientNodeName,
                                @Value("${erlang.recipientMailBoxName}") String recipientMailBoxName,
                                @Value("${erlang.maxAttempts}") Long maxAttempts,
                                @Value("${erlang.maxTimeout}") Long maxTimeOut
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

        this.MAX_ATTEMPTS=maxAttempts;
        this.MAX_TIMEOUT=maxTimeOut;

        randomGenerator=new Random();
        mapState=new MapState();
        initializeDispatcherNodeAvailables();
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

        switch (op) {

            case JS_OP_CODE_CREATE_MAP:
                response=createMapProtocol(mbox,request_body);
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



    public JSONObject createMapProtocol(OtpMbox mbox, JSONObject request_body) throws JSONException, OtpErlangDecodeException, OtpErlangExit {

        OtpErlangTuple erlangMsg = DispatcherConversionUtilities.createMapErlangRequest(mbox.self(), request_body);

        OtpErlangObject msg = basicErlangSendReceiveProtocol(mbox, erlangMsg);
        if (msg==null){
            JSONObject response =new JSONObject();
            response.put("answer",NEGATIVE_ANSWER);
            response.put("msg","Connection timeout! Try again later!");
            return response;
        }

        JSONObject response = DispatcherConversionUtilities.answerCreateMapFromErlang((OtpErlangTuple) msg);
        System.out.println("Answer received " + response.toString());

        return response;
    }



    public JSONObject selectDeselectProtocol( OtpMbox mbox, JSONObject request_body, String mailBoxRecipient, String serverNodeName) throws JSONException, OtpErlangDecodeException, OtpErlangExit {

        OtpErlangTuple erlangMsg = DispatcherConversionUtilities.selectDeselectErlangRequest(mbox.self(), request_body);
        OtpErlangObject msg = basicErlangSendReceiveProtocol(mbox, erlangMsg);
        if (msg==null){
            JSONObject response =new JSONObject();
            response.put("answer",NEGATIVE_ANSWER);
            response.put("msg","Connection timeout! Try again later!");
            return response;
        }
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
        OtpErlangObject msg = basicErlangSendReceiveProtocol(mbox, erlangMsg);
        if (msg==null){
            JSONObject response =new JSONObject();
            response.put("answer",NEGATIVE_ANSWER);
            response.put("msg","Connection timeout! Try again later!");
            return response;
        }
        JSONObject response = DispatcherConversionUtilities.answerShowMapFromErlang((OtpErlangTuple) msg);

        //Maybe here consider the possibility to store an hash of the entire map.
        System.out.println("Map returned. --");
        
        return response;
    }


    /**
     * The basic idea is the following: each time we send a request to a dispatcher we repeat that request at least MAX_ATTEMPTS
     * unless the answer is not null. If it reaches MAX_ATTEMPTS, we sign that that dispatcher is not available and it tries another one.
     * @param mbox
     * @param erlangMsg
     * @return
     * @throws OtpErlangDecodeException
     * @throws OtpErlangExit
     */
    private OtpErlangObject basicErlangSendReceiveProtocol(OtpMbox mbox,OtpErlangTuple erlangMsg) throws OtpErlangDecodeException, OtpErlangExit {

        OtpErlangObject msg=null;

        if(!isSomeDispatcherAvailable()){
            initializeDispatcherNodeAvailables();
        }

        while (isSomeDispatcherAvailable() && msg==null){
            int trials=0;
            String serverNodeName= getRandomDispatcherAvailable();
            System.out.println("Selected dispatcherNode: "+serverNodeName);
            while (!"".equals(serverNodeName) && msg==null && trials<MAX_ATTEMPTS ){

                System.out.println("Try to send msg trial nr: "+(trials+1));
                trials++;

                mbox.send(recipientMailBox,serverNodeName, erlangMsg);
                System.out.println("Request sent .. Wait for an answer.");

                //blocking receive operation
                msg = mbox.receive(MAX_TIMEOUT);
                System.out.println("Msg received .. "+msg);

            }
            if (trials==MAX_ATTEMPTS){
                System.out.println("Probably dispatcher "+serverNodeName + "is dead. Removed from list.");
                removeDispatcherFromList(serverNodeName);
            }
        }

        return msg;
    }



    public MapState getMapState() {
        return mapState;
    }



    private synchronized void initializeDispatcherNodeAvailables(){
        if (clientNode==null || dispatcherNodeNames.size()==dispatcherNodesAvailables.size()) return;
        for (String dispatcherName:dispatcherNodeNames){
            if (clientNode.ping(dispatcherName, MAX_TIMEOUT)){
                dispatcherNodesAvailables.add(dispatcherName);
            }
        }
    }

    private synchronized String getRandomDispatcherAvailable(){
        if (isSomeDispatcherAvailable()){
            int index = randomGenerator.nextInt(dispatcherNodesAvailables.size());
            return (String)(dispatcherNodesAvailables.toArray())[index];
        }
        return "";
    }

    private synchronized void removeDispatcherFromList(String dispatcherName){
        dispatcherNodesAvailables.remove(dispatcherName);
    }

    public synchronized boolean isSomeDispatcherAvailable(){
        return !dispatcherNodesAvailables.isEmpty();
    }



}
