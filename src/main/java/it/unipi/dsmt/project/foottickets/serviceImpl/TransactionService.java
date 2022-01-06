package it.unipi.dsmt.project.foottickets.serviceImpl;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.Transaction;
import it.unipi.dsmt.project.foottickets.repository.ITransactionRepository;
import it.unipi.dsmt.project.foottickets.service.ITransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service("mainTransactionService")
public class TransactionService implements ITransactionService {

    @Autowired
    private ITransactionRepository iTransactionRepository;

    @Override
    public List<Transaction> findLast10TransactionOfAccount(String accountUsername) {
        List<Transaction> transList=iTransactionRepository.findTop10ByAccount_UsernameOrderByTimestampDesc(accountUsername);
        return transList;
    }
}
