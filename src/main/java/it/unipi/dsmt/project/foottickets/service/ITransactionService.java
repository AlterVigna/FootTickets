package it.unipi.dsmt.project.foottickets.service;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.Transaction;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public interface ITransactionService {

    public List<Transaction> findLast10TransactionOfAccount(String accountUsername);
    //public List<Transaction> findAllById(Long id);
}
