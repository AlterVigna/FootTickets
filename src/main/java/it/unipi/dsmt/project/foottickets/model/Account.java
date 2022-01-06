package it.unipi.dsmt.project.foottickets.model;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


import javax.persistence.*;
import java.io.Serializable;
import java.util.Set;

@Entity
@Getter
@Setter
@Table(name="Account")
public class Account implements Serializable {

    @Id
    private String username;
    private String password;
    private Double amount;
    private String type;



    public Account() {

    }
}
