import './style.css';
import { render } from 'preact';
import { LocationProvider, Router, Route } from 'preact-iso';

import { Exhibit1 } from './pages/Exhibit1';
import { NotFound } from './pages/_404';

export function App() {
    return (
        <LocationProvider>
            <Router>
                <Route path="/exhibit-1" component={Exhibit1} />
                <Route default component={NotFound} />
            </Router>
        </LocationProvider>
    );
}

render(<App />, document.getElementById('app'));
