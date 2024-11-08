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

function showNotification(messageElement) {
  // TODO: show notifications in inactive chats
  // TODO: show chat in notification
  Notification.requestPermission().then((result) => {
    if (result === "granted") {
      const img = "img/logo.jpg";
      const author = messageElement.querySelector(
        ".message-header div strong small"
      ).innerText;
      // TODO: handle line breaks
      const content = messageElement.querySelector(
        ".message-content small"
      ).innerText;
      const notification = new Notification(author, {
        icon: img,
        body: content,
      });
    }
  });
}

function getUserSigil() {
  const user = JSON.parse(localStorage.getItem("user"));
  return user.sigil;
}

function parseIncomingWSMessageHTML(message) {
  const parser = new DOMParser();
  const doc = parser.parseFromString(message, "text/html");
  const messageElement = doc.body.firstElementChild;
  return messageElement;
}

function isOwnMessage(messageElement) {
  return messageElement.dataset.ownMessage !== undefined;
}

function getIncomingWSMessageType(messageElement) {
  return messageElement.dataset.messageType;
}

function handleIncomingWSMessage(message) {
  const messageElement = parseIncomingWSMessageHTML(message);
  const messageType = getIncomingWSMessageType(messageElement);
  switch (messageType) {
    case "old-messages":
      handleOldMessages(messageElement);
      break;
    case "new-message":
      handleNewMessage(messageElement);
      break;
    case "members":
      handleMembers(messageElement);
      break;
  }
}

function handleOldMessages(messageElement) {}

function handleNewMessage(messageElement) {
  if (isOwnMessage(messageElement)) {
    const messagesContainer = document.getElementById("messages");
    scrollToBottom(messagesContainer);
  } else {
    showNotification(messageElement);
  }
}

function handleMembers(messageElement) {}

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
