package it.unipi.dsmt.project.foottickets.configuration;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import static it.unipi.dsmt.project.foottickets.configuration.GlobalConfiguration.*;

// This annotation is necessary to indicate to spring that this object must be insert into the application Context
@Configuration
public class SecurityConfiguration extends WebSecurityConfigurerAdapter {

    @Autowired
    private UserDetailsService userDetailsService;

    @Bean
    public AuthenticationSuccessHandler myAuthenticationSuccessHandler(){
        return new CustomUrlAuthenticationSuccessHandler();
    }

    // This method specify what paths will be free and what paths will be protected.
    @Override
    protected void configure(HttpSecurity http) throws Exception {

            //To protect all the paths and resources from undesired users
            http.authorizeRequests()
                    .antMatchers("/home").authenticated()
                    .antMatchers("/buyer/**").hasRole(ROLE_BUYER_NAME)
                    .antMatchers("/admin/**").hasRole(ROLE_ADMIN_NAME)
                    .antMatchers("/").permitAll()
                    .and().formLogin()
                    .loginPage(LOGIN_CALL)
                    .failureUrl(LOGIN_CALL+"?credentialError=true")
                    .successHandler(myAuthenticationSuccessHandler())

                    .and().csrf().disable(); // This is an advanced control to undesired access to the apis.
    }


    // The aim of this method is to indicate to spring, what service should use to load the user.
    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {

        auth.userDetailsService(userDetailsService);
    }





}
