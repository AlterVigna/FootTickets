package it.unipi.dsmt.project.foottickets.serviceImpl;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.springframework.stereotype.Service;

import java.util.Optional;


// This is an example of how to switch an implementation of a service from another.
// Just adding a specific annotation to qualify which service will be implemented during dependency injection.
@Service
public class AccountServiceLOCAL implements IAccountService {

    @Override
    public Optional<Account> findByUsername(String username) {

        Optional<Account> ret = Optional.of(new Account());

        ret.get().setUsername("adam");
        ret.get().setPassword("ILoveJava");
        ret.get().setAmount(100.00);
        ret.get().setType("U");

        return Optional.of(ret.get());
    }

    @Override
    public boolean saveNewAccount(Account account) {
        // Fake save
        System.out.println("FAKE Entity saved..");
        return true;
    }


}
