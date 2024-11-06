function autoResize(element) {
  element.style.height = "auto";
  const scrollHeight = element.scrollHeight;
  const maxHeight = parseFloat(getComputedStyle(element).lineHeight) * 10; // 10 rows

  if (scrollHeight <= maxHeight) {
    element.style.height = scrollHeight + "px";
    element.style.overflowY = "hidden";
  } else {
    element.style.height = maxHeight + "px";
    element.style.overflowY = "auto";
  }
}

function showNotification(message) {
  // TODO: show notifications in inactive chats
  // TODO: show chat, author and message content in notification
  Notification.requestPermission().then((result) => {
    if (result === "granted") {
      const img = "img/logo.jpg";
      const notification = new Notification("New hbs2 message", {
        icon: img,
      });
      // const user = JSON.parse(localStorage.getItem("user"));
      // if (user.publicKey !== message.author) {
      //   const notification = new Notification(message.author, {
      //     body: message.body,
      //     icon: img,
      //   });
      // }
    }
  });
}

function getUserSigil() {
  const user = JSON.parse(localStorage.getItem("user"));
  return user.sigil;
}

function getIncomingWSMessageType(message) {
  const parser = new DOMParser();
  const doc = parser.parseFromString(message, "text/html");
  const messageContainer = doc.body.firstElementChild;
  return messageContainer.dataset.messageType;
}

function handleIncomingWSMessage(message) {
  const messageType = getIncomingWSMessageType(message);
  switch (messageType) {
    case "old-messages":
      handleOldMessages();
      break;
    case "new-message":
      handleNewMessage();
      break;
    case "members":
      handleMembers();
      break;
  }
}

function handleOldMessages(messages) {}

function handleNewMessage(message) {
  // TODO: don't show notifications when sending a message
  showNotification(message);
  // TODO: scroll down automatically when sending a message, see https://htmx.org/attributes/hx-swap/
  // const messagesContainer = document.getElementById("messages");
  // if (...) {
  //
  // }
}

function handleMembers(message) {}

function getOutgoingWSMessageType(message) {
  const messageObject = JSON.parse(message);
  return messageObject.type;
}

function handleOutgoingWSMessage(message) {
  const messageType = getOutgoingWSMessageType(message);
  switch (messageType) {
    case "message":
      handleMessage();
      break;
  }
}

function handleMessage(message) {}
