import { withAuth } from '@okta/okta-react';
import React, { Component } from 'react';
import { Container, Menu } from 'semantic-ui-react';
import { checkAuthentication } from './helpers';

export default withAuth(class Navbar extends Component {
    constructor(props) {
        super(props);
        this.state = { authenticated: null };

        this.login = this.login.bind(this);
        this.logout = this.logout.bind(this);
        this.checkAuthentication = checkAuthentication.bind(this);
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

    async logout() {
        this.props.auth.logout('/');
    }

    render() {
        const menuItems = this.state.authenticated === true ? [
            <Menu.Item key="profile" id="profile-button" as="a" href="/profile">Profile</Menu.Item>,
            <Menu.Item key="logout" id="logout-button" as="a" onClick={this.logout}>Logout</Menu.Item>
        ] : [ <Menu.Item key="login" as="a" onClick={this.login}>Login</Menu.Item> ];

        return (
            <div>
                <Menu fixed="top" inverted>
                    <Container>
                        <Menu.Item as="a" header href="/">
                            <i>vot</i>sec
                        </Menu.Item>

                        {menuItems}
                    </Container>
                </Menu>
            </div>
        );
    }
});
