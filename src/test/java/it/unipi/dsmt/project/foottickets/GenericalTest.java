package it.unipi.dsmt.project.foottickets;

import com.ericsson.otp.erlang.*;
import it.unipi.dsmt.project.foottickets.dto.MapDTO;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.POSITIVE_ANSWER;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class GenericalTest {

    @Test
    public void shouldAnswerWithTrue()
    {

        int rows=10;
        int cols=10;

        OtpErlangObject [] keys= new OtpErlangObject[rows*5];
        OtpErlangObject [] values= new OtpErlangObject[cols*5];

        for (int i=0;i<rows*5;i++){
            keys[i]=new OtpErlangTuple(new OtpErlangInt(i));
            values[i]=new OtpErlangTuple(new OtpErlangAtom("not_used"));
        }

        OtpErlangMap newMap= new OtpErlangMap(keys,values);
        System.out.println(newMap.toString());


        for (OtpErlangObject ErlPlace:newMap.values()) {

            System.out.println(ErlPlace.toString());

        }

        assertTrue( true );
    }


    @Test
    public void test1() throws Exception {


        Set<String> lockedPlaces=new HashSet<>();
        lockedPlaces.add("0_0");
        lockedPlaces.add("0_1");

        MapDTO map= new MapDTO();
        map.setAnswer(POSITIVE_ANSWER);
        map.setNumRows(2L);
        map.setNumCols(3L);
        map.setPrice(100L);
        map.setLockedPlaces(lockedPlaces);
        map.setResponseCode(200);
        map.setMessageDescription("");
        Set<String> selectedPlaces=new HashSet<>();
        selectedPlaces.add("0_1");
        map.setCurrentSelectedPlaces(selectedPlaces);
        JSONObject responseBody=map.toJSON();


    }

    /*private void testLogin(String birthday, String ss) throws Exception {
        MvcResult result = (MvcResult) mockMvc.perform(MockMvcRequestBuilders.post("/login")
                        .with(user("TEST_USER_ID"))
                        .with(csrf())
                        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(view().name(LOGIN_PAGE));

        String resultSS = result.getResponse().getContentAsString();
        assertNotNull(resultSS);
        assertEquals(ss, resultSS);
    }*/



}
