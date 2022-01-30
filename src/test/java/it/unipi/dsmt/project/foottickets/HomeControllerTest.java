package it.unipi.dsmt.project.foottickets;

import it.unipi.dsmt.project.foottickets.controller.HomeController;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.core.Authentication;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


/**
 *
 * https://developer.okta.com/blog/2019/03/28/test-java-spring-boot-junit5
 *
 * For the most part, unit tests are intended to test a small chunk (or unit) of code.
 * That is usually limited to the code within a function or sometimes extends to some helper functions called from that function.
 * If a unit test is testing code that is dependent on another service or resource, like a database or a network resource,
 * the unit test should “mock” and inject that dependency as to have no actual impact on that external resource.
 * It also limits the focus to just that unit being tested.
 * To mock a dependency, you can either use a mock library like “Mockito” or simply pass in a different
 * implementation of the dependency that you want to replace.
 *
 *
 * Integration tests are intended to test the entire integrated code path (from end-to-end) for a specific use-case.
 */


@SpringBootTest
@AutoConfigureMockMvc
public class HomeControllerTest {


    @MockBean
    private DispatcherInterface dispatcherInterface;

    @MockBean
    @Qualifier("mainAccountService")
    private IAccountService accountService;


    @Autowired
    private HomeController controller;

    @Autowired
    private MockMvc mockMvc;



    @Test
    public void contextLoads() {
        assertThat(controller).isNotNull();
    }



    @Test
    public void testLoginPage() throws Exception {
        this.mockMvc.perform(get("/login")).andExpect(status().isOk())
                .andExpect(model().attribute("account", instanceOf(Account.class)))
               // .andExpect(MockMvcResultMatchers.model().attribute("Account",instanceOf(Account.class)))
                .andExpect(view().name(LOGIN_PAGE));
    }

    @Test
    public void testSignIn1() throws Exception {

        when (accountService.saveNewAccount(ArgumentMatchers.any())).thenReturn(true);
        Account accTest=new Account();

        this.mockMvc.perform(MockMvcRequestBuilders.post("/signIn")
                .param("username","pippo")
                .param("password","prova")
                .param("amount","150")
                .param("type","U"))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:"+LOGIN_CALL+"?registrationOk=true"));

    }

    @Test
    public void testSignIn2() throws Exception {

        when (accountService.saveNewAccount(ArgumentMatchers.any())).thenReturn(false);
        Account accTest=new Account();

        this.mockMvc.perform(MockMvcRequestBuilders.post("/signIn")
                        .param("username","pippo")
                        .param("password","prova")
                        .param("amount","150")
                        .param("type","U"))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:"+LOGIN_CALL+"?registrationOk=false"));
    }

    @Test
    public void testRedirect1_WithoutAUTH() throws Exception {

        //when (accountService.saveNewAccount(ArgumentMatchers.any())).thenReturn(false);
        Account accTest=new Account();

        this.mockMvc.perform(MockMvcRequestBuilders.get("/"))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:"+LOGIN_CALL));
    }

    @Test
    @WithMockUser("customUsername")
    public void testRedirect1_Buyer() throws Exception {

        //when (accountService.saveNewAccount(ArgumentMatchers.any())).thenReturn(false);
        Account accTest=new Account();
        accTest.setType(CODE_ROLE_BUYER);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/").sessionAttr(KEY_CURRENT_USER,accTest))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:"+HOME_BUYER));
    }

    @Test
    @WithMockUser("customUsername")
    public void testRedirect2_Admin() throws Exception {

        //when (accountService.saveNewAccount(ArgumentMatchers.any())).thenReturn(false);
        Account accTest=new Account();
        accTest.setType(CODE_ROLE_ADMIN);

        this.mockMvc.perform(MockMvcRequestBuilders.get("/").sessionAttr(KEY_CURRENT_USER,accTest))
                .andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:"+HOME_ADMIN));
    }



}
