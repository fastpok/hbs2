function autoResize(innerElement, outerElement) {
  outerElement.style.height = "auto";
  const scrollHeight = innerElement.scrollHeight;
  const maxHeight = parseFloat(getComputedStyle(innerElement).lineHeight) * 10; // 10 rows
  if (scrollHeight <= maxHeight) {
    outerElement.style.height = scrollHeight + "px";
    innerElement.style.overflowY = "hidden";
  } else {
    outerElement.style.height = maxHeight + "px";
    innerElement.style.overflowY = "auto";
  }
}

function scrollToBottom(element) {
  element.scrollTop = element.scrollHeight;
}

function showNotification(messageElement) {
  // TODO: show notifications in inactive chats
  // TODO: show chat name in notification
  Notification.requestPermission().then((result) => {
    if (result === "granted") {
      const img = "img/logo.jpg";
      const author = messageElement.querySelector(
        ".message-header div strong small"
      ).innerText;
      // // TODO: handle line breaks
      // const content = messageElement.querySelector(
      //   ".message-content small"
      // ).innerText;
      const title = "New message from " + author;
      const notification = new Notification(title, { icon: img });
    }
  });
}

function getUserSigil() {
  const user = JSON.parse(localStorage.getItem("user"));
  return user.sigil;
}

// NOTE: we use htmx swap-oob here, so the the encapsulating tags are stripped
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

function handleOldMessages(messageElement) {
  if (messageElement.dataset.initialBatch !== undefined) {
    // removes spinners
    document.getElementById("messages").removeAttribute("aria-busy");
    document.getElementById("members").removeAttribute("aria-busy");
  }
}

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

async function handleFilesMessageWSConfigSend(event) {
  // Unfortunately, there is no such event for websockets as htmx:confirm (https://htmx.org/events/#htmx:confirm),
  // so we cancel event and call socketWrapper.send manually
  event.preventDefault();
  const newFilesMessageBody = await getNewFilesMessageBody(
    event.detail.parameters
  );
  event.detail.socketWrapper.send(newFilesMessageBody, event.detail.elt);
}

async function getNewFilesMessageBody(eventParams) {
  const files = eventParams.filesUpload;
  let newMessage;
  const readFilePromises = [];
  if (Array.isArray(files)) {
    files.forEach((file) => readFilePromises.push(readFileAsDataURL(file)));
    newMessage = await Promise.all(readFilePromises);
  } else {
    newMessage = [await readFileAsDataURL(files)];
  }
  const newMessageBody = {
    type: eventParams.type,
    message: newMessage,
  };
  return JSON.stringify(newMessageBody);
}

function readFileAsDataURL(file) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => {
      const result = {
        dataURL: reader.result,
        filename: file.name,
      };
      resolve(result);
    };
    reader.onerror = () => {
      reject(new Error("Failed to read the file"));
    };
    reader.readAsDataURL(file);
  });
}

function handlePaste(event) {
  const files = event.clipboardData.files;
  if (files.length > 0) {
    document.getElementById("files-message-form").reset();
    const filesInput = document.getElementById("files-input");
    filesInput.files = files;
    const filesInputModal = document.getElementById("send-files-modal");
    if (!filesInputModal.open) {
      openModal(filesInputModal);
      document.getElementById("send-files-submit-button").focus();
    }
  }
}

// TODO: this is ugly, think of a better way to implement header menu
function handleWindowResize() {
  function getInitialStyles() {
    return {
      wrapper: document.querySelector(".wrapper").style.gridTemplateColumns,
      sidebarHeader: document.querySelector(".sidebar-header").style.display,
      sidebar: document.querySelector(".sidebar").style.display,
      contentHeader: document.querySelector(".content-header").style.display,
      content: document.querySelector(".content").style.display,
      membersHeader: document.querySelector(".members-header").style.display,
      members: document.querySelector(".members").style.display,
    };
  }

  function applyStyles(styles) {
    document.querySelector(".wrapper").style.gridTemplateColumns =
      styles.wrapper;
    document.querySelector(".sidebar-header").style.display =
      styles.sidebarHeader;
    document.querySelector(".sidebar").style.display = styles.sidebar;
    document.querySelector(".content-header").style.display =
      styles.contentHeader;
    document.querySelector(".content").style.display = styles.content;
    document.querySelector(".members-header").style.display =
      styles.membersHeader;
    document.querySelector(".members").style.display = styles.members;
  }

  const mediaQueryList = window.matchMedia("(min-width: 768px)");

  let initialStyles = getInitialStyles();
  mediaQueryList.addEventListener("change", () => {
    if (mediaQueryList.matches) {
      initialStyles = getInitialStyles();
      applyStyles({
        wrapper: "1fr 4fr 1fr",
        sidebarHeader: "flex",
        sidebar: "flex",
        contentHeader: "flex",
        content: "flex",
        membersHeader: "flex",
        members: "block",
      });
    } else {
      applyStyles({
        wrapper: initialStyles.wrapper,
        sidebarHeader: initialStyles.sidebarHeader,
        sidebar: initialStyles.sidebar,
        contentHeader: initialStyles.contentHeader,
        content: initialStyles.content,
        membersHeader: initialStyles.membersHeader,
        members: initialStyles.members,
      });
    }
  });
}
