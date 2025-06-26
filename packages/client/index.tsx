import React from 'react'
import ReactDOM from 'react-dom/client'
import Footer  from '@monorepo/footer'
import Header from './modules/header';
import Main from './modules/main';
import './index.css';

const App = () => (
    <main>
        <Header/>
        <Main/>
        <Footer/>
    </main>

);

const root = ReactDOM.createRoot(document.getElementById('root'));

root.render(<App/>)
