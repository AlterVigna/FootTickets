package it.unipi.dsmt.project.foottickets.service;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.Transaction;

import java.util.List;


public interface ITransactionService {

    public List<Transaction> findLast10TransactionOfAccount(String accountUsername);
    public void saveTransactionAndUpdateAccount(Transaction t, Account a) throws Exception;

}
