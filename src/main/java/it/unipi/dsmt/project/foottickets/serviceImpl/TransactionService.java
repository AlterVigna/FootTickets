package it.unipi.dsmt.project.foottickets.serviceImpl;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.Transaction;
import it.unipi.dsmt.project.foottickets.repository.IAccountRepository;
import it.unipi.dsmt.project.foottickets.repository.ITransactionRepository;
import it.unipi.dsmt.project.foottickets.service.ITransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Optional;

@Service("mainTransactionService")
@Transactional(rollbackOn = {Exception.class})
public class TransactionService implements ITransactionService {

    @Autowired
    private IAccountRepository iAccountRepository;

    @Autowired
    private ITransactionRepository iTransactionRepository;

    @Override
    public List<Transaction> findLast10TransactionOfAccount(String accountUsername) {
        List<Transaction> transList=iTransactionRepository.findTop10ByAccount_UsernameOrderByTimestampDesc(accountUsername);
        return transList;
    }

    @Override
    public void saveTransactionAndUpdateAccount(Transaction t, Account a) throws Exception {

            Optional<Account> current_account = iAccountRepository.findAccountByUsername(a.getUsername());
            if (current_account.isPresent()){
                // Here I'm in a transactional method. I check again if amount is changed.
                if (t.getPrice()>current_account.get().getAmount()){
                    throw new Exception();
                }
                iTransactionRepository.save(t);

                Double currentAmount = current_account.get().getAmount();
                currentAmount=currentAmount-t.getPrice();
                current_account.get().setAmount(currentAmount);
                iAccountRepository.save(current_account.get());
            }
    }

}
