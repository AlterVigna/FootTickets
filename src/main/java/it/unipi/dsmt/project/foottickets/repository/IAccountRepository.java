package it.unipi.dsmt.project.foottickets.repository;

import it.unipi.dsmt.project.foottickets.model.Account;
import org.springframework.data.repository.CrudRepository;

import java.util.Optional;

// This interface will provide the methods to access and modify data about Account table in database.
// We have to indicate the Class table we want to implement and the type of the primary key.
// The implementation class of the above methods will be created by Spring at runtime automatically.
// See this link for more details https://www.concretepage.com/spring-5/spring-data-crudrepository-example
public interface IAccountRepository extends CrudRepository<Account,String> {

    public Optional<Account> findAccountByUsername(String username);


}
