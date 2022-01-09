package it.unipi.dsmt.project.foottickets.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;


/**
 * This DTO represents the fields form of web page for creating a new seats map.
 */

@Getter
@Setter
public class CreateMapDTO {

    Integer numRows;
    Integer numCols;
    Integer price;
    List<String> selectedPlaces=new ArrayList<>();

}
