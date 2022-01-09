package it.unipi.dsmt.project.foottickets.model;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;

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
