package it.unipi.dsmt.project.foottickets.repository;

import it.unipi.dsmt.project.foottickets.model.Transaction;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface ITransactionRepository extends CrudRepository<Transaction,Integer> {

    // Equivalent to do
    // SELECT *
    // FROM TRANSACTION
    // WHERE ACCOUNT= ?
    // ORDER BY TIMESTAMP DESC
    // LIMIT 10
    public List<Transaction> findTop10ByAccount_UsernameOrderByTimestampDesc(String account);


}
