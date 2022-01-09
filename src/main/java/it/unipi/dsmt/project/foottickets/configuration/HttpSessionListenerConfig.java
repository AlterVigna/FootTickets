package it.unipi.dsmt.project.foottickets.configuration;

import it.unipi.dsmt.project.foottickets.model.Account;

import javax.servlet.annotation.WebListener;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;
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
     * This method will be automatically called when session destroyed
     *
     * @param sessionEvent
     */
    @Override
    public void sessionDestroyed(HttpSessionEvent sessionEvent) {
        //  LOG.info("-------Decrementing Session Counter--------");

        Set<String> selectedPlaces = (Set<String>) sessionEvent.getSession().getAttribute(KEY_SELECTED_SEATS);
        Account account = (Account) sessionEvent.getSession().getAttribute(KEY_CURRENT_USER);
        Integer seatPrice = (Integer) sessionEvent.getSession().getAttribute(KEY_SEAT_COST);

        // TODO call to dispatcer for removing  selected seats and make them available.
        activeSessions.decrementAndGet();
        sessionEvent.getSession().setAttribute("activeSessions", activeSessions.get());
        // LOG.info("-------Session Destroyed--------");
    }
}