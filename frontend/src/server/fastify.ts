/*
server.addHook("onRequest", async (req, res) => {
  console.log(req.url);
  if (req.url !== "/") {
    return;
  }
  const context = await getUserSession(req);

  if (!context.user) return res.redirect("/login");
  if (context.game && context.room)
    return res.redirect(`/game/${context.room.id}`);
  return res.redirect("/lobby");
});
*/
