async function checkAuthentication() {
    const authenticated = await this.props.auth.isAuthenticated();

    if (this.state.authenticated !== authenticated) {
        if (authenticated && !this.state.userinfo) {
            this.setState({
                authenticated,
                userinfo: await this.props.auth.getUser()
            });
        } else {
            this.setState({ authenticated });
        }
    }
}

export { checkAuthentication };
