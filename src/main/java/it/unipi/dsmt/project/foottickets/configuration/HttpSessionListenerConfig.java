package it.unipi.dsmt.project.foottickets.configuration;

import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.TempTransaction;
import it.unipi.dsmt.project.foottickets.service.ITempTransactionService;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;

import javax.servlet.annotation.WebListener;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

// https://www.techgeeknext.com/spring-boot-session-listener
// Add interceptor for session management.

// The @WebListener annotation is used to register a class as a listener of a web application.
// This class is used for manage the session.
@WebListener
public class HttpSessionListenerConfig implements HttpSessionListener {


    private final AtomicInteger activeSessions;

    public HttpSessionListenerConfig() {
        super();
        activeSessions = new AtomicInteger();
    }


    /**
     * This method will be called when session created
     *
     * @param sessionEvent
     */
    @Override
    public void sessionCreated(HttpSessionEvent sessionEvent) {
        // LOG.info("-------Incrementing Session Counter--------");

        activeSessions.incrementAndGet();
        //LOG.info("-------Session Created--------");
        sessionEvent.getSession().setAttribute("activeSessions", activeSessions.get());
        // LOG.info("Total Active Session : {} ", activeSessions.get());
    }

    /**
     * This method will be automatically called when session destroyed.
     *
     * @param sessionEvent
     */
    @Override
    public void sessionDestroyed(HttpSessionEvent sessionEvent) {
        //  LOG.info("-------Decrementing Session Counter--------");

        // Another way to retrieve bean inside the application context.
        WebApplicationContext context = WebApplicationContextUtils.getWebApplicationContext(sessionEvent.getSession().getServletContext());
        DispatcherInterface di = (DispatcherInterface) context.getBean("dispatcherInterface");
        ITempTransactionService itts= (ITempTransactionService) context.getBean("mainTempTransactionService");

        Account username= (Account) sessionEvent.getSession().getAttribute(KEY_CURRENT_USER);
        Set<String> selectedPlaces = (Set<String>) sessionEvent.getSession().getAttribute(KEY_SELECTED_SEATS);
        if (selectedPlaces!=null){
            for (String selPlace: selectedPlaces) {

                JSONObject jsonRequest=new JSONObject();
                try {
                    jsonRequest.put("operation",JS_OP_CODE_DESELECT_PLACE);
                    jsonRequest.put("placeSelected",selPlace);
                    di.executeClientTask(jsonRequest);

                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        Optional<TempTransaction> tempTrans = itts.findTempTransactionAccount(username.getUsername());
        if (tempTrans.isPresent()){
            itts.removeTempTransaction(tempTrans.get());
        }
        activeSessions.decrementAndGet();
        sessionEvent.getSession().setAttribute("activeSessions", activeSessions.get());
        System.out.println("Session Destroyed!");
        // LOG.info("-------Session Destroyed--------");
    }


    private DispatcherInterface getDispatcherInterface(HttpSessionEvent se) {
        WebApplicationContext context = WebApplicationContextUtils.getWebApplicationContext(se.getSession().getServletContext());
        return (DispatcherInterface) context.getBean("dispatcherInterface");
    }


}