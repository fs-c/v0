import { withAuth } from '@okta/okta-react';
import React, { Component } from 'react';
import { Button, Header } from 'semantic-ui-react';
import { checkAuthentication } from './helpers';

export default withAuth(class Home extends Component {
    constructor(props) {
        super(props);
        this.state = { authenticated: null, userinfo: null };
        this.checkAuthentication = checkAuthentication.bind(this);
        this.login = this.login.bind(this);
    }

    async componentDidMount() {
        this.checkAuthentication();
    }

    async componentDidUpdate() {
        this.checkAuthentication();
    }

    async login() {
        this.props.auth.login('/');
    }

    render() {
        if (this.state.authenticated === null)
            return <div></div>;
        
        return (<div>
             <Header as="h1"><i>vot</i>sec index</Header>

            <div>
                {this.state.authenticated ? (<div>
                    <p>Welcome back, {this.state.userinfo.name}!</p>
                </div>) : (<div>
                    <p>
                        Optimally there'll be some text telling you more about this 
                        project in the future.
                    </p>

                    <Button id="login-button" primary onClick={this.login}>
                        Login</Button>
                </div>)}
            </div>
        </div>);
    }
});
