function processProgramTextToModel(text){
  const parsedInfo = {};
  parsedInfo["walls"] = [];
  parsedInfo["targets"] = [];

  const parts = text.split("\n");
  for(let part of parts){
    if(part.length>0){
      spart = part.split(" ");
      switch(part[0]){
        case "s":
          parsedInfo["size"] = {
            "x": parseInt(spart[1]),
            "y": parseInt(spart[2])
          };
          break;
        case "w":
          parsedInfo["walls"].push({
            "sx": parseInt(spart[1]),
            "sy": parseInt(spart[2]),
            "x": parseInt(spart[3]),
            "y": parseInt(spart[4])
          });
          break;
        case "t":
          parsedInfo["targets"].push({
            "x": parseInt(spart[1]),
            "y": parseInt(spart[2])
          });
          break;
        case "b":
          parsedInfo["robot"] = {
            "x": parseInt(spart[1]),
            "y": parseInt(spart[2])
          };
          break;
        case "i":
          parsedInfo["instructions"] = spart[1].split("");
          break;
        case "#":
        default:
          break;
      }
    }
  }

  return parsedInfo;
}