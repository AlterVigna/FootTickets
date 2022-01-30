package it.unipi.dsmt.project.foottickets.repository;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.TempTransaction;
import org.springframework.data.repository.CrudRepository;


import java.util.Optional;

public interface ITempTransactionRepository extends CrudRepository<TempTransaction,Integer> {


    public Optional<TempTransaction> findByAccount(Account account);



}
