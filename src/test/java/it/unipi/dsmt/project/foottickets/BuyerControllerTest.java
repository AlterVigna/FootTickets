package it.unipi.dsmt.project.foottickets;

import it.unipi.dsmt.project.foottickets.controller.AdminController;
import it.unipi.dsmt.project.foottickets.controller.BuyerController;
import it.unipi.dsmt.project.foottickets.dto.CreateMapDTO;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.DispatcherInterface;
import it.unipi.dsmt.project.foottickets.erlangInterfaces.MapState;
import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.TempTransaction;
import it.unipi.dsmt.project.foottickets.model.Transaction;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import it.unipi.dsmt.project.foottickets.service.ITempTransactionService;
import it.unipi.dsmt.project.foottickets.service.ITransactionService;
import org.hamcrest.Matchers;
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

import java.util.*;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;




@SpringBootTest
@AutoConfigureMockMvc
public class BuyerControllerTest {


    @MockBean
    private DispatcherInterface dispatcherInterface;

    @MockBean
    @Qualifier("mainAccountService")
    private IAccountService accountService;

    @MockBean
    @Qualifier("mainTransactionService")
    ITransactionService transactionService;

    @MockBean
    @Qualifier("mainTempTransactionService")
    ITempTransactionService iTempTransactionService;

    @Autowired
    private BuyerController controller;

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    MockHttpServletRequest request;


    @Test
    public void contextLoads() {
        assertThat(controller).isNotNull();
    }


    @Test
    @WithMockUser(roles="BUYER")
    public void testViewPage_NoPreviousSelections() throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest();

        Account nuovo= new Account();
        TempTransaction tempTransaction= new TempTransaction();

        when (accountService.findByUsername(ArgumentMatchers.any())).thenReturn(Optional.of(nuovo));
        when (iTempTransactionService.findTempTransactionAccount(ArgumentMatchers.any())).thenReturn(Optional.empty());

        this.mockMvc.perform(get("/buyer").sessionAttr("request",request)).andExpect(status().isOk())
                .andExpect(request().sessionAttribute(KEY_CURRENT_USER,nuovo))
                .andExpect(view().name(HOME_BUYER_PAGE));

        assertNotNull(request);

    }


    @Test
    @WithMockUser(roles="BUYER")
    public void testViewPage_WithPreviousSelections() throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest();

        Account nuovo= new Account();
        TempTransaction tempTransaction= new TempTransaction();
        tempTransaction.setLocation("0_0;0_1");
        when (accountService.findByUsername(ArgumentMatchers.any())).thenReturn(Optional.of(nuovo));
        when (iTempTransactionService.findTempTransactionAccount(ArgumentMatchers.any())).thenReturn(Optional.of(tempTransaction));
        Set<String> set= new HashSet<>();
        set.add("0_0");
        set.add("0_1");
        this.mockMvc.perform(get("/buyer").sessionAttr("request",request)).andExpect(status().isOk())
                .andExpect(request().sessionAttribute(KEY_CURRENT_USER,nuovo))
                .andExpect(request().sessionAttribute(KEY_SELECTED_SEATS,set))
                .andExpect(view().name(HOME_BUYER_PAGE));

        assertNotNull(request);
    }


    @Test
    @WithMockUser(roles="BUYER")
    public void testViewPurchased() throws Exception {

        Account buyer= new Account();
        buyer.setUsername("trial");

        List<Transaction> exampleList=new ArrayList<>();
        Transaction t1 = new Transaction();
        t1.setNumSeats(1);
        t1.setLocation("0_0");
        exampleList.add(t1);
        Transaction t2 = new Transaction();
        t2.setNumSeats(2);
        t2.setLocation("1_1;1_2");
        exampleList.add(t2);
        when (transactionService.findLast10TransactionOfAccount(ArgumentMatchers.any())).thenReturn(exampleList);
        this.mockMvc.perform(get("/buyer/viewPurchased").sessionAttr("request",request)
                        .sessionAttr(KEY_CURRENT_USER,buyer)).andExpect(status().isOk())

                .andExpect( model().attribute("listTransactions", instanceOf(List.class)))
                .andExpect( model().attribute("listTransactions", hasSize(2)))
                .andExpect(view().name(VIEW_PURCHASED_PAGE));
    }


    @Test
    @WithMockUser(roles="BUYER")
    public void testBuyTicketMap() throws Exception {

        this.mockMvc.perform(get("/buyer/buyTicket"))
                .andExpect(view().name(BUY_TICKET_VIEW));
    }


    @Test
    @WithMockUser(roles="BUYER")
    public void testExecuteBuy_No_SelectedSeats() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        Set<String> set = new HashSet<>();

        this.mockMvc.perform(MockMvcRequestBuilders.post("/buyer/executeBuy").sessionAttr("request",request).sessionAttr(KEY_SELECTED_SEATS, set)
                ).andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:" + HOME_BUYER + "/buyTicket?errorNoSelectedSeat=" + true));
    }


    @Test
    @WithMockUser(roles="BUYER")
    public void testExecuteBuy_No_MoneyAvailable() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        Set<String> set = new HashSet<>();
        set.add("0_0");
        set.add("0_1");

        Account nuovo= new Account();
        nuovo.setAmount(10.00);
        when (accountService.findByUsername(ArgumentMatchers.any())).thenReturn(Optional.of(nuovo));

        MapState newMap=new MapState();
        newMap.setPrice(500L);
        when (dispatcherInterface.getMapState()).thenReturn(newMap);

        this.mockMvc.perform(MockMvcRequestBuilders.post("/buyer/executeBuy")
                        .sessionAttr("request",request)
                        .sessionAttr(KEY_CURRENT_USER,nuovo)
                        .sessionAttr(KEY_SELECTED_SEATS, set)
                ).andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:"+HOME_BUYER+"/buyTicket?errorNoMoney="+true));
    }

    @Test
    @WithMockUser(roles="BUYER")
    public void testExecuteBuy_Ok() throws Exception {

        MockHttpServletRequest request = new MockHttpServletRequest();

        Set<String> set = new HashSet<>();
        set.add("0_0");
        set.add("0_1");

        Account nuovo= new Account();
        nuovo.setAmount(1000.00);
        when (accountService.findByUsername(ArgumentMatchers.any())).thenReturn(Optional.of(nuovo));
        //when (transactionService.saveTransactionAndUpdateAccount(ArgumentMatchers.,ArgumentMatchers.any())).thenReturn(null);

        MapState newMap=new MapState();
        newMap.setPrice(500L);
        when (dispatcherInterface.getMapState()).thenReturn(newMap);

        this.mockMvc.perform(MockMvcRequestBuilders.post("/buyer/executeBuy")
                        .sessionAttr("request",request)
                        .sessionAttr(KEY_CURRENT_USER,nuovo)
                        .sessionAttr(KEY_SELECTED_SEATS, set)
                ).andExpect(status().is3xxRedirection())
                .andExpect(view().name("redirect:"+HOME_BUYER+"?purchasedTicket=true"));
    }





}
