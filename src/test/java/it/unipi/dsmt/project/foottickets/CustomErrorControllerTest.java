package it.unipi.dsmt.project.foottickets;

import it.unipi.dsmt.project.foottickets.controller.AdminController;
import it.unipi.dsmt.project.foottickets.controller.CustomErrorController;
import it.unipi.dsmt.project.foottickets.dto.CreateMapDTO;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.MapState;
import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import javax.servlet.RequestDispatcher;
import java.util.Optional;

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
public class CustomErrorControllerTest {


    @Autowired
    private CustomErrorController controller;

    @Autowired
    private MockMvc mockMvc;



    @Test
    public void contextLoads() {
        assertThat(controller).isNotNull();
    }



    @Test
    @WithMockUser(roles="ADMIN")
    public void testHandleError403() throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest();

        Exception ex = new Exception();
        this.mockMvc.perform(get("/error")
                        .requestAttr("javax.servlet.error.exception",ex)
                        .requestAttr(RequestDispatcher.ERROR_STATUS_CODE,403)
                        .sessionAttr("request",request)).andExpect(status().isOk())
                .andExpect(view().name(ERROR_403));

    }


    @Test
    @WithMockUser(roles="ADMIN")
    public void testHandleError404() throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest();

        Exception ex = new Exception();
        this.mockMvc.perform(get("/error")
                        .requestAttr("javax.servlet.error.exception",ex)
                        .requestAttr(RequestDispatcher.ERROR_STATUS_CODE,404)
                        .sessionAttr("request",request)).andExpect(status().isOk())
                .andExpect(view().name(ERROR_404));

    }


    @Test
    @WithMockUser(roles="ADMIN")
    public void testHandleError500() throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest();

        NullPointerException ex = new NullPointerException();
        this.mockMvc.perform(get("/error")
                        .requestAttr("javax.servlet.error.exception",ex)
                        .requestAttr(RequestDispatcher.ERROR_STATUS_CODE,500)
                        .sessionAttr("request",request)).andExpect(status().isOk())
                .andExpect(view().name(ERROR_500));

    }





}
