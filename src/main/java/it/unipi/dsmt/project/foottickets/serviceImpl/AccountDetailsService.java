package it.unipi.dsmt.project.foottickets.serviceImpl;

import it.unipi.dsmt.project.foottickets.model.Account;
import it.unipi.dsmt.project.foottickets.service.IAccountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;


// This class is used for spring security.
@Service
public class AccountDetailsService implements UserDetailsService {



    @Autowired
    @Qualifier("mainAccountService")
    IAccountService accountService;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

        Optional<Account> user = accountService.findByUsername(username);


        return new UserDetails() {
            @Override
            public Collection<? extends GrantedAuthority> getAuthorities() {

                List<SimpleGrantedAuthority> authorities= new ArrayList<SimpleGrantedAuthority>();

                if (user.isPresent() && CODE_ROLE_BUYER.equals(user.get().getType())){
                    authorities.add(new SimpleGrantedAuthority(ROLE_BUYER));
                }
                if (user.isPresent() && CODE_ROLE_ADMIN.equals(user.get().getType())){
                    authorities.add(new SimpleGrantedAuthority(ROLE_ADMIN));
                }


                return authorities;
            }

            @Override
            public String getPassword() {

                if (user.isPresent()){
                    return "{noop}"+user.get().getPassword();
                }
                return "{noop}"+WRONG_PASSWORD; // This avoid that empty user log in.
                // The password are stored encrypted for privacy reason. So we have to indicate the encoder.
                // For accademic reasons we are supposing that the password are not crypted in this way. We don't use
                // any encoder.
            }

            @Override
            public String getUsername() {
                return username;
            }

            @Override
            public boolean isAccountNonExpired() {
                return true;
            }

            @Override
            public boolean isAccountNonLocked() {
                return true;
            }

            @Override
            public boolean isCredentialsNonExpired() {
                return true;
            }

            @Override
            public boolean isEnabled() {
                return true;
            }
        };
    }
}
