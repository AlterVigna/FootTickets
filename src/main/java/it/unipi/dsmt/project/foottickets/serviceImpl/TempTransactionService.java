package it.unipi.dsmt.project.foottickets.serviceImpl;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.model.TempTransaction;
import it.unipi.dsmt.project.foottickets.model.Transaction;
import it.unipi.dsmt.project.foottickets.repository.IAccountRepository;
import it.unipi.dsmt.project.foottickets.repository.ITempTransactionRepository;
import it.unipi.dsmt.project.foottickets.repository.ITransactionRepository;
import it.unipi.dsmt.project.foottickets.service.ITempTransactionService;
import it.unipi.dsmt.project.foottickets.service.ITransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Optional;

@Service("mainTempTransactionService")
@Transactional(rollbackOn = {Exception.class})
public class TempTransactionService implements ITempTransactionService {

    @Autowired
    private ITempTransactionRepository iTempTempransactionRepository;

    @Autowired
    private IAccountRepository iAccountRepository;


    @Override
    public void saveTempTransaction(TempTransaction t, String accountUsername) throws Exception {
        Optional<Account> current_account = iAccountRepository.findAccountByUsername(accountUsername);
        if (current_account.isPresent()){
            t.setAccount(current_account.get());
            iTempTempransactionRepository.save(t);
        }
    }

    @Override
    public void removeTempTransaction(TempTransaction t) {
        iTempTempransactionRepository.delete(t);
    }

    @Override
    public Optional<TempTransaction> findTempTransactionAccount(String accountUsername) {
        Optional<Account> current_account = iAccountRepository.findAccountByUsername(accountUsername);
        Optional<TempTransaction> tempTransaction= Optional.of(new TempTransaction());

        if(current_account.isPresent()){
            tempTransaction=iTempTempransactionRepository.findByAccount(current_account.get());
        }

        return tempTransaction;
    }
}
