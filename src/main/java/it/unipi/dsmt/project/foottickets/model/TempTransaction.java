package it.unipi.dsmt.project.foottickets.model;


import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;
import java.time.LocalDateTime;

@Entity
@Getter
@Setter
@Table(name="TempTransaction")
public class TempTransaction implements Serializable {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;
    @Column(name="numseats")
    private Integer numSeats;
    @Column(name="location")
    private String location;
    @Column(name="price")
    private Double price;

    @Column(name="timestamp", insertable = false, updatable = false)
    private LocalDateTime timestamp;

    @ManyToOne
    @JoinColumn(name = "username")
    private Account account;

}
