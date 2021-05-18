use serde_json::Value;

#[tokio::test]
async fn exit() -> anyhow::Result<()> {
    let service = &mut crate::testing::service::spawn()?.0;

    // send "initialize" request
    crate::assert_status!(service, Ok(()));
    let request = &crate::testing::lsp::initialize::request();
    let response = Some(crate::testing::lsp::initialize::response());
    crate::assert_exchange!(service, request, Ok(response));

    // send "initialized" notification
    crate::assert_status!(service, Ok(()));
    let notification = &crate::testing::lsp::initialized::notification();
    let status = None::<Value>;
    crate::assert_exchange!(service, notification, Ok(status));

    // send "exit" notification
    crate::assert_status!(service, Ok(()));
    let notification = &crate::testing::lsp::exit::notification();
    let status = None::<Value>;
    crate::assert_exchange!(service, notification, Ok(status));

    // send "textDocument/didOpen" notification; should error
    crate::assert_status!(service, Err(lspower::ExitedError));
    let notification = &{
        let uri = lsp::Url::parse("inmemory::///test")?;
        let language_id = "ddlog.dl";
        let version = 1;
        let text = String::from("");
        crate::testing::lsp::text_document::did_open::notification(&uri, language_id, version, text)
    };
    let status = lspower::ExitedError;
    crate::assert_exchange!(service, notification, Err(status));

    Ok(())
}

#[tokio::test]
async fn initialize() -> anyhow::Result<()> {
    let service = &mut crate::testing::service::spawn()?.0;

    // send "initialize" request
    crate::assert_status!(service, Ok(()));
    let request = &crate::testing::lsp::initialize::request();
    let response = Some(crate::testing::lsp::initialize::response());
    crate::assert_exchange!(service, request, Ok(response));

    Ok(())
}

#[tokio::test]
async fn initialized() -> anyhow::Result<()> {
    let service = &mut crate::testing::service::spawn()?.0;

    // send "initialize" request
    crate::assert_status!(service, Ok(()));
    let request = &crate::testing::lsp::initialize::request();
    let response = Some(crate::testing::lsp::initialize::response());
    crate::assert_exchange!(service, request, Ok(response));

    // send "initialized" notification
    crate::assert_status!(service, Ok(()));
    let notification = &crate::testing::lsp::initialized::notification();
    let status = None;
    crate::assert_exchange!(service, notification, Ok(status));

    Ok(())
}

#[tokio::test]
async fn initialize_once() -> anyhow::Result<()> {
    let service = &mut crate::testing::service::spawn()?.0;

    // send "initialize" request
    crate::assert_status!(service, Ok(()));
    let request = &crate::testing::lsp::initialize::request();
    let response = Some(crate::testing::lsp::initialize::response());
    crate::assert_exchange!(service, request, Ok(response));

    // send "initialized" notification
    crate::assert_status!(service, Ok(()));
    let notification = &crate::testing::lsp::initialized::notification();
    let status = None::<Value>;
    crate::assert_exchange!(service, notification, Ok(status));

    // send "initialize" request (again); should error
    crate::assert_status!(service, Ok(()));
    let request = &crate::testing::lsp::initialize::request();
    let response = Some(crate::testing::jsonrpc::error::invalid_request());
    crate::assert_exchange!(service, request, Ok(response));

    Ok(())
}

#[tokio::test]
async fn shutdown() -> anyhow::Result<()> {
    let service = &mut crate::testing::service::spawn()?.0;

    // send "initialize" request
    crate::assert_status!(service, Ok(()));
    let request = &crate::testing::lsp::initialize::request();
    let response = Some(crate::testing::lsp::initialize::response());
    crate::assert_exchange!(service, request, Ok(response));

    // send "initialized" notification
    crate::assert_status!(service, Ok(()));
    let notification = &crate::testing::lsp::initialized::notification();
    let status = None::<Value>;
    crate::assert_exchange!(service, notification, Ok(status));

    // send "shutdown" request
    crate::assert_status!(service, Ok(()));
    let request = &crate::testing::lsp::shutdown::request();
    let response = Some(crate::testing::lsp::shutdown::response());
    crate::assert_exchange!(service, request, Ok(response));

    // send "shutdown" request (again); should error
    crate::assert_status!(service, Ok(()));
    let request = &crate::testing::lsp::shutdown::request();
    let response = Some(crate::testing::jsonrpc::error::invalid_request());
    crate::assert_exchange!(service, request, Ok(response));

    // send "exit" notification
    crate::assert_status!(service, Ok(()));
    let notification = &crate::testing::lsp::exit::notification();
    let status = None::<Value>;
    crate::assert_exchange!(service, notification, Ok(status));

    Ok(())
}

mod text_document {
    use futures::stream::StreamExt;
    use serde_json::Value;

    #[tokio::test]
    async fn did_close() -> anyhow::Result<()> {
        let uri = lsp::Url::parse("inmemory:///test")?;
        let language_id = "ddlog.dl";
        let version = 1;
        // FIXME: panic if text is empty string
        let text = String::from("input relation Rin(b: bool)\n");

        let (ref mut service, ref mut messages) = crate::testing::service::spawn()?;

        // send "initialize" request
        crate::assert_status!(service, Ok(()));
        let request = &crate::testing::lsp::initialize::request();
        let response = Some(crate::testing::lsp::initialize::response());
        crate::assert_exchange!(service, request, Ok(response));

        // send "initialized" notification
        crate::assert_status!(service, Ok(()));
        let notification = &crate::testing::lsp::initialized::notification();
        let status = None::<Value>;
        crate::assert_exchange!(service, notification, Ok(status));
        // ignore the "window/logMessage" notification: "WebAssembly language server initialized!"
        messages.next().await.unwrap();

        // send "textDocument/didOpen" notification for `uri`
        crate::assert_status!(service, Ok(()));
        let notification = &crate::testing::lsp::text_document::did_open::notification(&uri, language_id, version, text);
        let status = None::<Value>;
        crate::assert_exchange!(service, notification, Ok(status));

        // receive "textDocument/publishDiagnostics" notification for `uri`
        let message = messages.next().await.unwrap();
        let actual = serde_json::to_value(&message)?;
        let expected = {
            let diagnostics = &[];
            crate::testing::lsp::text_document::publish_diagnostics::notification(&uri, diagnostics)
        };
        assert_eq!(actual, expected);

        // send "textDocument/didClose" notification for `uri`
        crate::assert_status!(service, Ok(()));
        let notification = &crate::testing::lsp::text_document::did_close::notification(&uri);
        let status = None::<Value>;
        crate::assert_exchange!(service, notification, Ok(status));

        // send "shutdown" request
        crate::assert_status!(service, Ok(()));
        let request = &crate::testing::lsp::shutdown::request();
        let response = Some(crate::testing::lsp::shutdown::response());
        crate::assert_exchange!(service, request, Ok(response));

        // send "exit" notification
        crate::assert_status!(service, Ok(()));
        let notification = &crate::testing::lsp::exit::notification();
        let status = None::<Value>;
        crate::assert_exchange!(service, notification, Ok(status));

        Ok(())
    }

    #[tokio::test]
    async fn did_open() -> anyhow::Result<()> {
        let uri = lsp::Url::parse("inmemory:///test")?;
        let language_id = "ddlog.dl";
        let version = 1;
        let text = String::from("input relation Rin(b: bool)\n");

        let (ref mut service, ref mut messages) = crate::testing::service::spawn()?;

        // send "initialize" request
        crate::assert_status!(service, Ok(()));
        let request = &crate::testing::lsp::initialize::request();
        let response = Some(crate::testing::lsp::initialize::response());
        crate::assert_exchange!(service, request, Ok(response));

        // send "initialized" notification
        crate::assert_status!(service, Ok(()));
        let notification = &crate::testing::lsp::initialized::notification();
        let status = None::<Value>;
        crate::assert_exchange!(service, notification, Ok(status));
        // ignore the "window/logMessage" notification: "WebAssembly language server initialized!"
        messages.next().await.unwrap();

        // send "textDocument/didOpen" notification for `uri`
        crate::assert_status!(service, Ok(()));
        let notification = &crate::testing::lsp::text_document::did_open::notification(&uri, language_id, version, text);
        let status = None::<Value>;
        crate::assert_exchange!(service, notification, Ok(status));

        // receive "textDocument/publishDiagnostics" notification for `uri`
        let message = messages.next().await.unwrap();
        let actual = serde_json::to_value(&message)?;
        let expected = {
            let diagnostics = &[];
            crate::testing::lsp::text_document::publish_diagnostics::notification(&uri, diagnostics)
        };
        assert_eq!(actual, expected);

        // send "shutdown" request
        crate::assert_status!(service, Ok(()));
        let request = &crate::testing::lsp::shutdown::request();
        let response = Some(crate::testing::lsp::shutdown::response());
        crate::assert_exchange!(service, request, Ok(response));

        // send "exit" notification
        crate::assert_status!(service, Ok(()));
        let notification = &crate::testing::lsp::exit::notification();
        let status = None::<Value>;
        crate::assert_exchange!(service, notification, Ok(status));

        Ok(())
    }
}
