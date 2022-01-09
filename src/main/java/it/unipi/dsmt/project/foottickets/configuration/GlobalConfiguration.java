package it.unipi.dsmt.project.foottickets.configuration;

public class GlobalConfiguration {

    // References to web pages show to web browsers.
    public static final String ERROR_403="errors/error-403";
    public static final String ERROR_404="errors/error-404";
    public static final String ERROR_500="errors/error-500";
    public static final String LOGIN_PAGE="login";
    public static final String HOME_ADMIN_PAGE="homeA";
    public static final String HOME_BUYER_PAGE="homeB";
    public static final String VIEW_PURCHASED_PAGE="viewPurchased";
    public static final String CREATE_MAP_VIEW="createMap";
    public static final String VIEW_MAP_VIEW="viewMap";
    public static final String BUY_TICKET_VIEW="buyTicket";

    // Paths for calling controller methods
    public static final String LOGIN_CALL="/login";
    public static final String HOME_ADMIN="/admin";
    public static final String HOME_BUYER="/buyer";


    // Roles - for spring security config.
    public static final String ROLE_ADMIN="ROLE_ADMIN";
    public static final String ROLE_BUYER="ROLE_BUYER";
    public static final String ROLE_ADMIN_NAME="ADMIN";
    public static final String ROLE_BUYER_NAME="BUYER";
    public static final String CODE_ROLE_ADMIN="A";
    public static final String CODE_ROLE_BUYER="B";


    // Session Keyword
    public static final String KEY_CURRENT_USER="current_user";
    public static final String KEY_SELECTED_SEATS="selected_seats";
    public static final String KEY_SEAT_COST="seat_cost";


    // Other parameter
    public static final Double DEFAULT_NEW_AMOUNT=500.00;
    public static final String WRONG_PASSWORD="§§§__WRONG__PASSWORD__4SURE__§§";

    public static int POSITIVE_ANSWER=1;
    public static int NEGATIVE_ANSWER=0;

}
