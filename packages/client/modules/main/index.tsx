import React from 'react';

function Main() {
  return (
    <>
      <article>
        <section
          className="bordered"
          style={{ flexDirection: "row", justifyContent: "space-between", gap: "5px" }}
        >
          <div style={{ justifySelf: "center" }}>
            <img
              height="120px" width="105px"
              style={{ border: "solid 1px" }}
              src="./assets/avatar.jpg"
            />
          </div>
          <div>
            <img width="20" src="./assets/icons/nix.svg" />{" "}
            Nix enthusiast<br />
            Experienced in
            <ul style={{ "paddingInlineStart": "15px" }}>
              <li>Software development</li>
              <li>Data processing</li>
              <li>Linux and DevOps</li>
            </ul>
          </div>
        </section>
      </article>
      <article className="projects">
        <section className="bordered">
          <h3>
            Company#1:{" "}
            <img width="24" src="./assets/icons/haskell.svg" />{" "}
            <img width="24" src="./assets/icons/clickhouse.svg" />{" "}
            <img width="24" src="./assets/icons/javascript.svg" />{" "}
            <img width="24" src="./assets/icons/postgresql.svg" />{" "}
            <img width="24" src="./assets/icons/docker.svg" />
          </h3>
          <p>I started my career on an AdTech project with a 3-year history as a Software developer</p>
          <ul>
            <li>
              Optimized infrastructure<br />
              <b>224CPU/304Gb {'->'} 24CPU/32Gb</b>
            </li>
            <li>
              Built real-time analytics<br />
              <b>30m {'->'} 5s delay</b>
            </li>
            <li>
              Improved release frequency<br />
              <b>1/week {'->'} 2-4/day</b>
            </li>
            <li>Improved <b>SLA 85% {'->'} +98%</b></li>
            <li>Hired 1 developer</li>
            <li>Managed a team of 4 people</li>
          </ul>
        </section>
        <section className="bordered">
          <h3>
            Company#2:{" "}
            <img width="24" src="./assets/icons/javascript.svg" />{" "}
            <img width="24" src="./assets/icons/postgresql.svg" />{" "}
            <img width="24" src="./assets/icons/k8s.svg" />{" "}
            <img width="24" src="./assets/icons/argocd.svg" />{" "}
            <img width="24" src="./assets/icons/prometheus.svg" />
          </h3>
          <p>Continued my career as an Infrastructure engineer</p>
          <ul>
            <li>
              Optimized AWS EKS cost<br />
              <b>8000$ {'->'} 4000$ monthly bill</b>
            </li>
            <li>Built automated QA environment</li>
            <li>Implemented daily databases backups
              and its testing
            </li>
            <li>Configured metrics and alerts</li>
          </ul>
        </section>
      </article>
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
