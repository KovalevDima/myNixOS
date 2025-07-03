import React from 'react';

function Main() {
  return (
    <>
      <article className="projects">
        <section className="bordered">
          <h3>
            <a href="https://clickhaskell.dev/">ClickHaskell</a>:{" "}
            <img width="24" src="./assets/icons/haskell.svg" />{" "}
            <img width="24" src="./assets/icons/clickhouse.svg" />{" "}
            <img width="24" src="./assets/icons/nix.svg" />
          </h3>
          <p>Implemented ClickHouse DBMS <b>Native protocol</b> and <b>client</b> in Haskell</p>
          <ul>
            <li>Compile time verification</li>
            <li>Low dependency footprint</li>
            <li>Documentation as a code</li>
            <li>Rich CI/CD and QA</li>
          </ul>
        </section>
        <section className="bordered">
          <h3>
            <a href="https://github.com/KovalevDima/KovalevDima">Homelab</a>:{" "}
            <img width="24" src="./assets/icons/nix.svg" />{" "}
            <img width="24" src="./assets/icons/haskell.svg" />{" "}
            <img width="24" src="./assets/icons/bitcoin.svg" />{" "}
            <img height="24" src="./assets/icons/btcpay-server.svg" />{" "}
            <img height="24" src="./assets/icons/keycloak.svg" />
          </h3>
          <p>Selfhosting with best <b>Everything as a Code</b> approach tool Nix</p>
          <ul>
            <li>IaC for this page</li>
            <li>Bitcoin node and BTCPay server</li>
            <li>Full disk encryption</li>
            <li>Tiling window manager</li>
            <li>Keycloak IAM service</li>
          </ul>
        </section>
      </article>
    </>
  );
}
export default Main;
