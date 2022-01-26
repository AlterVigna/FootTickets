package it.unipi.dsmt.project.foottickets;

import com.ericsson.otp.erlang.*;
import org.junit.jupiter.api.Test;

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

}
