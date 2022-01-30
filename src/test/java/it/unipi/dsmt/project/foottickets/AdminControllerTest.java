package it.unipi.dsmt.project.foottickets;

import it.unipi.dsmt.project.foottickets.controller.AdminController;
import it.unipi.dsmt.project.foottickets.controller.HomeController;
import it.unipi.dsmt.project.foottickets.dto.CreateMapDTO;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.MapState;
import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import javax.servlet.http.HttpServletRequest;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mockitoSession;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;



@SpringBootTest
@AutoConfigureMockMvc
public class AdminControllerTest {


    @MockBean
    private DispatcherInterface dispatcherInterface;

    @MockBean
    @Qualifier("mainAccountService")
    private IAccountService accountService;


    @Autowired
    private AdminController controller;

    @Autowired
    private MockMvc mockMvc;



    @Test
    public void contextLoads() {
        assertThat(controller).isNotNull();
    }


    @Test
    @WithMockUser(roles="ADMIN")
    public void testViewMap() throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest();

        Account nuovo= new Account();
        when (accountService.findByUsername(ArgumentMatchers.any())).thenReturn(Optional.of(nuovo));
        this.mockMvc.perform(get("/admin").sessionAttr("request",request)).andExpect(status().isOk())
                .andExpect(request().sessionAttribute(KEY_CURRENT_USER,nuovo))
                .andExpect(view().name(HOME_ADMIN_PAGE));

        assertNotNull(request);

    }

    @Test
    @WithMockUser(roles="ADMIN")
    public void testHomeAdminPage() throws Exception {

        this.mockMvc.perform(get("/admin/viewMap")).andExpect(status().isOk())
                .andExpect(view().name(VIEW_MAP_VIEW));
    }

    @Test
    @WithMockUser(roles="ADMIN")
    public void testCreateMap() throws Exception {

        this.mockMvc.perform(get("/admin/createMap")).andExpect(status().isOk())
                .andExpect(model().attribute("mapForm", instanceOf(CreateMapDTO.class)))
                .andExpect(view().name(CREATE_MAP_VIEW));
    }

    @Test
    @WithMockUser(roles="ADMIN")
    public void testConfirmMap_OK() throws Exception{
        JSONObject responseJson=new JSONObject();
        responseJson.put("answer",POSITIVE_ANSWER);
        responseJson.put("hash","hash");
        responseJson.put("msg","msg");
        MapState newMap=new MapState();
        when (dispatcherInterface.getMapState()).thenReturn(newMap);
        when (dispatcherInterface.executeClientTask(ArgumentMatchers.any())).thenReturn(responseJson);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/admin/confirmMap")
                        .param("numRows","3")
                        .param("numCols","5")
                        .param("price","150")
                        .param("selectedPlaces","")
                ).andExpect(status().is3xxRedirection())
                .andExpect(view().name( "redirect:"+HOME_ADMIN+"?mapCreated=true&optionalMsg="));

        assertEquals(newMap.getHash(), "hash");
        assertEquals(newMap.getNumRows(), 3L);
        assertEquals(newMap.getNumCols(), 5L);
        assertEquals(newMap.getPrice(), 150L);
        assertNotNull(newMap.getLockedPlaces());
        assertEquals(newMap.getLockedPlaces().size(),0);

    }

    @Test
    @WithMockUser(roles="ADMIN")
    public void testConfirmMap_OK_WITH_PLACE_SELECTED() throws Exception{

        JSONObject responseJson=new JSONObject();
        responseJson.put("answer",POSITIVE_ANSWER);
        responseJson.put("hash","hash");
        responseJson.put("msg","msg");
        MapState newMap=new MapState();

        when (dispatcherInterface.getMapState()).thenReturn(newMap);
        when (dispatcherInterface.executeClientTask(ArgumentMatchers.any())).thenReturn(responseJson);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/admin/confirmMap")
                        .param("numRows","3")
                        .param("numCols","5")
                        .param("price","150")
                        .param("selectedPlaces","0_0")
                ).andExpect(status().is3xxRedirection())
                .andExpect(view().name( "redirect:"+HOME_ADMIN+"?mapCreated=true&optionalMsg="));

        assertEquals(newMap.getHash(), "hash");
        assertEquals(newMap.getNumRows(), 3L);
        assertEquals(newMap.getNumCols(), 5L);
        assertEquals(newMap.getPrice(), 150L);
        assertNotNull(newMap.getLockedPlaces());
        assertEquals(newMap.getLockedPlaces().size(),1);

    }


    @Test
    @WithMockUser(roles="ADMIN")
    public void testConfirmMap_NEG_ANSWER() throws Exception{

        JSONObject responseJson=new JSONObject();
        responseJson.put("answer",NEGATIVE_ANSWER);
        responseJson.put("hash","hash");
        responseJson.put("msg","msg");
        MapState newMap=new MapState();

        when (dispatcherInterface.getMapState()).thenReturn(newMap);
        when (dispatcherInterface.executeClientTask(ArgumentMatchers.any())).thenReturn(responseJson);
        this.mockMvc.perform(MockMvcRequestBuilders.post("/admin/confirmMap")
                        .param("numRows","3")
                        .param("numCols","5")
                        .param("price","150")
                        .param("selectedPlaces","0_0")
                ).andExpect(status().is3xxRedirection())
                .andExpect(view().name( "redirect:"+HOME_ADMIN+"?mapCreated=false&optionalMsg=msg"));
    }


}
