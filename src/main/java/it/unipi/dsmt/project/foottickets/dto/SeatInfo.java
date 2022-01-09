package it.unipi.dsmt.project.foottickets.dto;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 * This DTO represent the information from the web client.
 */
@Getter
@Setter
public class SeatInfo implements Serializable {

    public String operation;
    public String placeSelected;


}
