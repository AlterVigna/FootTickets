package it.unipi.dsmt.project.foottickets.serviceImpl;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.repository.IAccountRepository;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service("mainAccountService")
public class AccountService implements IAccountService {

    @Autowired
    private IAccountRepository accountRepository;

    @Override
    public Optional<Account> findByUsername(String username) {
        if (username==null || username.trim().equals("")) return Optional.empty();
        return accountRepository.findAccountByUsername(username);
    }

    @Override
    public boolean saveAccount(Account account) {

        if (account!=null){

            Optional<Account> result = accountRepository.findAccountByUsername(account.getUsername());
            if (!result.isPresent()){
                Account a = accountRepository.save(account);
                if (a!=null) return true;
                return false;
            }
        }
        return false;
    }





}
