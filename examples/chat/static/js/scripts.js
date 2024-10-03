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

function isElementScrolledToBottom(element) {
  return (
    Math.abs(element.scrollHeight - element.scrollTop - element.clientHeight) <=
    1
  );
}

function scrollToBottom(element) {
  element.scrollTop = element.scrollHeight;
}

function showNotification(message) {
  Notification.requestPermission().then((result) => {
    if (result === "granted") {
      const img = "img/logo.jpg";
      const lastMessage = message.data.json[message.data.json.length - 1];
      const user = JSON.parse(localStorage.getItem("user"));
      if (user.publicKey !== lastMessage.author) {
        const notification = new Notification(lastMessage.author, {
          body: lastMessage.body,
          icon: img,
        });
      }
    }
  });
}
