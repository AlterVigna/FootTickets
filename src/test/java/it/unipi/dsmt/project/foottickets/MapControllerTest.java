package it.unipi.dsmt.project.foottickets;

import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dsmt.project.foottickets.controller.AdminController;
import it.unipi.dsmt.project.foottickets.controller.MapController;
import it.unipi.dsmt.project.foottickets.dto.CreateMapDTO;
import it.unipi.dsmt.project.foottickets.dto.MapDTO;
import it.unipi.dsmt.project.foottickets.dto.SeatInfo;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.MapState;
import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


@SpringBootTest
@AutoConfigureMockMvc
public class MapControllerTest {


    @MockBean
    private DispatcherInterface dispatcherInterface;

    @MockBean
    @Qualifier("mainAccountService")
    private IAccountService accountService;


    @Autowired
    private MapController controller;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    ObjectMapper objectMapper;

    @Test
    public void contextLoads() {
        assertThat(controller).isNotNull();
    }


    @Test
    public void testGetCompleteMap_POSITIVE_ANSWER() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        MapState newMap=new MapState();
        newMap.setHash("hash1");
        newMap.setPrice(100L);
        newMap.setNumRows(3L);
        newMap.setNumCols(5L);
        Set<String> lockedPlaces=new HashSet<>();
        lockedPlaces.add("0_0");
        lockedPlaces.add("0_1");
        newMap.setLockedPlaces(lockedPlaces);

        when (dispatcherInterface.getMapState()).thenReturn(newMap);

        JSONObject responseJson= new JSONObject();
        responseJson.put("answer",POSITIVE_ANSWER);
        responseJson.put("hash","hash2");
        responseJson.put("numRows",2);
        responseJson.put("numCols",3);
        responseJson.put("price",100L);

        JSONArray jsonArray= new JSONArray();
        jsonArray.put("0_0");
        jsonArray.put("0_1");
        responseJson.put("lockedPlaces",jsonArray);

        when (dispatcherInterface.executeClientTask(ArgumentMatchers.any())).thenReturn(responseJson);

        MapDTO map= new MapDTO();
        map.setAnswer(POSITIVE_ANSWER);
        map.setNumRows(2L);
        map.setNumCols(3L);
        map.setPrice(100L);
        map.setLockedPlaces(lockedPlaces);
        map.setResponseCode(0);
        //map.setMessageDescription("");


        /*Set<String> selectedPlaces=new HashSet<>();
        selectedPlaces.add("0_1");
        map.setCurrentSelectedPlaces(selectedPlaces);*/

        JSONObject responseBody=map.toJSON();

        this.mockMvc.perform(get("/rest/map")
                .sessionAttr("request",request)
                .accept(MediaType.APPLICATION_JSON).content(""))
                .andExpect(status().isOk())
                .andExpect(content().json(responseBody.toString()));


    }


    @Test
    public void testGetCompleteMap_HASH_MATCHES() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        MapState newMap=new MapState();
        newMap.setHash("hash1");
        newMap.setPrice(100L);
        newMap.setNumRows(3L);
        newMap.setNumCols(5L);
        Set<String> lockedPlaces=new HashSet<>();
        lockedPlaces.add("0_0");
        lockedPlaces.add("0_1");
        newMap.setLockedPlaces(lockedPlaces);

        when (dispatcherInterface.getMapState()).thenReturn(newMap);

        JSONObject responseJson= new JSONObject();
        responseJson.put("answer",HASH_MATCHES);
        responseJson.put("hash","hash1");
        responseJson.put("numRows",2);
        responseJson.put("numCols",3);
        responseJson.put("price",100L);

        JSONArray jsonArray= new JSONArray();
        jsonArray.put("0_0");
        jsonArray.put("0_1");
        responseJson.put("lockedPlaces",jsonArray);

        when (dispatcherInterface.executeClientTask(ArgumentMatchers.any())).thenReturn(responseJson);

        MapDTO map= new MapDTO();
        map.setAnswer(POSITIVE_ANSWER);
        map.setNumRows(3L);
        map.setNumCols(5L);
        map.setPrice(100L);
        map.setLockedPlaces(lockedPlaces);
        map.setResponseCode(0);


        JSONObject responseBody=map.toJSON();

        this.mockMvc.perform(get("/rest/map")
                        .sessionAttr("request",request)
                        .accept(MediaType.APPLICATION_JSON).content(""))
                .andExpect(status().isOk())
                .andExpect(content().json(responseBody.toString()));
    }


    @Test
    public void testGetCompleteMap_NEGATIVE_ANSWER() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        MapState newMap=new MapState();
        newMap.setHash("hash1");
        newMap.setPrice(100L);
        newMap.setNumRows(3L);
        newMap.setNumCols(5L);
        Set<String> lockedPlaces=new HashSet<>();
        lockedPlaces.add("0_0");
        lockedPlaces.add("0_1");
        newMap.setLockedPlaces(lockedPlaces);

        when (dispatcherInterface.getMapState()).thenReturn(newMap);

        JSONObject responseJson= new JSONObject();
        responseJson.put("answer",NEGATIVE_ANSWER);
        responseJson.put("hash","hash1");
        responseJson.put("numRows",2);
        responseJson.put("numCols",3);
        responseJson.put("price",100L);
        responseJson.put("msg","error");

        JSONArray jsonArray= new JSONArray();
        jsonArray.put("0_0");
        jsonArray.put("0_1");
        responseJson.put("lockedPlaces",jsonArray);

        when (dispatcherInterface.executeClientTask(ArgumentMatchers.any())).thenReturn(responseJson);

        MapDTO map= new MapDTO();
        map.setAnswer(NEGATIVE_ANSWER);
        map.setNumRows(3L);
        map.setNumCols(5L);
        map.setPrice(100L);
        map.setLockedPlaces(lockedPlaces);
        map.setResponseCode(0);
        map.setMessageDescription("error");


        JSONObject responseBody=map.toJSON();

        this.mockMvc.perform(get("/rest/map")
                        .sessionAttr("request",request)
                        .accept(MediaType.APPLICATION_JSON).content(""))
                .andExpect(status().isOk())
                .andExpect(content().json(responseBody.toString()));
    }



    @Test
    public void testReserveAPlace_NotAuthentication() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        MapDTO map= new MapDTO();
        map.setAnswer(NEGATIVE_ANSWER);
        map.setMessageDescription("Authentication is needed.");
        map.setResponseCode(HttpStatus.UNAUTHORIZED.value());
        map.setNumCols(0L);
        map.setNumRows(0L);
        map.setPrice(0L);

        Set<String> selectedPlaces=new HashSet<>();
        map.setCurrentSelectedPlaces(selectedPlaces);

        Set<String> lockedPlaces=new HashSet<>();
        map.setLockedPlaces(lockedPlaces);

        JSONObject responseBody=map.toJSON();

        SeatInfo newSeat= new SeatInfo(JS_OP_CODE_SELECT_PLACE,"0_0");


        this.mockMvc.perform(MockMvcRequestBuilders.post("/rest/reserveSeat")
                        .sessionAttr("request",request)
                        .accept(MediaType.APPLICATION_JSON).content(newSeat.toJSON().toString()))
                .andExpect(status().isUnauthorized())
                .andExpect(content().json(responseBody.toString()));
    }



    @Test
    @WithMockUser(roles="BUYER")
    public void testReserveAPlace_GeneralError() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        MapDTO map= new MapDTO();
        map.setAnswer(NEGATIVE_ANSWER);
        map.setMessageDescription("Parameter are not correct.");
        map.setResponseCode(HttpStatus.BAD_REQUEST.value());
        map.setNumCols(0L);
        map.setNumRows(0L);
        map.setPrice(0L);

        Set<String> selectedPlaces=new HashSet<>();
        map.setCurrentSelectedPlaces(selectedPlaces);

        Set<String> lockedPlaces=new HashSet<>();
        map.setLockedPlaces(lockedPlaces);

        JSONObject responseBody=map.toJSON();

        SeatInfo newSeat= new SeatInfo(null,"0_0");


        this.mockMvc.perform(MockMvcRequestBuilders.post("/rest/reserveSeat")
                        .sessionAttr("request",request)
                        .accept(MediaType.APPLICATION_JSON).content(newSeat.toJSON().toString()))
                .andExpect(status().isBadRequest())
                .andExpect(content().json(responseBody.toString()));
    }


    @Test
    @WithMockUser(roles="BUYER")
    public void testReserveAPlace_POSITIVE_ANSWER() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        MapDTO map= new MapDTO();
        map.setAnswer(POSITIVE_ANSWER);
        map.setMessageDescription("");
        map.setNumCols(0L);
        map.setNumRows(0L);
        map.setPrice(0L);

        Set<String> selectedPlaces=new HashSet<>();
        map.setCurrentSelectedPlaces(selectedPlaces);

        Set<String> lockedPlaces=new HashSet<>();
        map.setLockedPlaces(lockedPlaces);

        JSONObject responseBody=map.toJSON();

        SeatInfo newSeat= new SeatInfo(JS_OP_CODE_SELECT_PLACE,"0_0");


        Set<String> set= new HashSet<>();
        set.add("0_0");
        set.add("0_1");

        this.mockMvc.perform(MockMvcRequestBuilders.post("/rest/reserveSeat")
                        .sessionAttr("request",request)
                        .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(newSeat)));
                //.andExpect(status().isOk())
                //.andExpect(request().sessionAttribute(KEY_SELECTED_SEATS,set))
                //.andExpect(content().json(responseBody.toString()));
    }




}
