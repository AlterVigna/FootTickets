package it.unipi.dsmt.project.foottickets.service;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.TempTransaction;

import java.util.Optional;


public interface ITempTransactionService {

    public Optional<TempTransaction> findTempTransactionAccount(String accountUsername);
    public void saveTempTransaction(TempTransaction t, String accountUsername) throws Exception;
    public void removeTempTransaction(TempTransaction t);
}
