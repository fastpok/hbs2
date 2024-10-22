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

function scrollToBottom(element) {
  element.scrollTop = element.scrollHeight;
}

function showNotification(messages) {
  // TODO: show notifications in inactive chats
  // TODO: show chat, author and message content in notification
  Notification.requestPermission().then((result) => {
    if (result === "granted") {
      const img = "img/logo.jpg";
      const notification = new Notification("New hbs2 messages", {
        icon: img,
      });
      // const lastMessage = message.data.json[message.data.json.length - 1];
      // const user = JSON.parse(localStorage.getItem("user"));
      // if (user.publicKey !== lastMessage.author) {
      //   const notification = new Notification(lastMessage.author, {
      //     body: lastMessage.body,
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
    case "new-messages":
      handleNewMessages();
      break;
    case "members":
      handleMembers();
      break;
  }
}

function handleOldMessages(messages) {}

function handleNewMessages(messages) {
  // TODO: don't show notifications when sending a message
  showNotification(messages);
  // TODO: scroll down automatically when sending a message
  // const messagesContainer = document.getElementById("messages");
  // if (...) {
  //   scrollToBottom(messagesContainer);
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
