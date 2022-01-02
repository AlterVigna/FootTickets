package it.unipi.dsmt.project.foottickets.service;


import it.unipi.dsmt.project.foottickets.model.Account;

import java.util.Optional;

public interface IAccountService {

    public Optional<Account> findByUsername(String username);
    public boolean saveAccount(Account account);




}
