import React, { Component } from 'react';
import { withAuth } from '@okta/okta-react';
import { Header, Table } from 'semantic-ui-react';

import { checkAuthentication } from './helpers';

export default withAuth(class Profile extends Component {
    constructor(props) {
        super(props);
        this.state = { userinfo: null, ready: false };
        this.checkAuthentication = checkAuthentication.bind(this);
    }

    async componentDidMount() {
        await this.checkAuthentication();
        this.applyClaims();
    }

    async componentDidUpdate() {
        await this.checkAuthentication();
        this.applyClaims();
    }

    async applyClaims() {
        if (this.state.userinfo && !this.state.claims) {
            const claims = Object.entries(this.state.userinfo);
            this.setState({ claims, ready: true });
        }
    }

    render() {
        if (!this.state.ready)
            return <div><p>Fetching user profile...</p></div>;

        return (
            <div>
                <div>
                    <Header as="h1">
                        Profile information
                    </Header>

                    <Table>
                        <thead>
                            <tr>
                                <th>Claim</th>
                                <th>Value</th>
                            </tr>
                        </thead>
                        <tbody>
                            {this.state.claims.map((claimEntry) => {
                                const claimName = claimEntry[0];
                                const claimValue = claimEntry[1];
                                const claimId = `claim-${claimName}`;
                                return <tr key={claimName}>
                                    <td>{claimName}</td>
                                    <td id={claimId}>{claimValue}</td>
                                </tr>;
                            })}
                        </tbody>
                    </Table>
                </div>
            </div>
        );
    }
});
