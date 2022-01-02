package it.unipi.dsmt.project.foottickets.serviceImpl;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.repository.IAccountRepository;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class AccountServiceLOCAL implements IAccountService {

    @Override
    public Optional<Account> findByUsername(String username) {

        return Optional.empty();
    }

    @Override
    public boolean saveAccount(Account account) {
        // Fake save
        System.out.println("FAKE Entity saved..");
        return true;
    }

    public Optional<Account> findByUsernameAndPassword(String username, String password){

        Optional<Account> ritorno = Optional.of(new Account());

        ritorno.get().setUsername("adam");
        ritorno.get().setPassword("ILoveJava");
        ritorno.get().setAmount(100.00);
        ritorno.get().setType("U");

        return ritorno;
    }




}
