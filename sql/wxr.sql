 CREATE TABLE IF NOT EXISTS `GPlayer` (
  `PlayerID` varchar(100) NOT NULL COMMENT 'UID',
  `IsLoadCg` tinyint(4) DEFAULT '0' COMMENT '是否播放开场动画0播放1不播放',
  `IsLoadTip` tinyint(4) DEFAULT '0' COMMENT '是否播放新手提示0播放1不播放',
  `IsFirstGame` tinyint(4) DEFAULT '0' COMMENT '是否是第一次结算',
  `IsLoadGameTip` tinyint(4) DEFAULT '0' COMMENT '是否播放单人新手提示0播放1不播放',
  `Title` smallint(2) unsigned DEFAULT '0' COMMENT '称号',
  `Name` varchar(100) DEFAULT '' COMMENT '昵称',
  `Head` varchar(300) DEFAULT '' COMMENT '头像',
  `Gender` varchar(100) DEFAULT '' COMMENT '性别',
  `City` varchar(100) DEFAULT '' COMMENT '市',
  `Province` varchar(100) DEFAULT '' COMMENT '省',
  `Country` varchar(100) DEFAULT '' COMMENT '国家',
  `Exp` int(10) unsigned DEFAULT '0' COMMENT '经验',
  `Gold` int(10) unsigned DEFAULT '0' COMMENT '金币',
  `Score` int(10) unsigned DEFAULT '0' COMMENT '积分',
  `CurPiFu` smallint(1) unsigned DEFAULT '1' COMMENT '皮肤',
  `PiFu` varchar(100) DEFAULT '[]' COMMENT '皮肤',
  `GetedGoalReward` varchar(100) DEFAULT '[]' COMMENT '已领取的协同目标',
  `Star` int(10) unsigned DEFAULT '0' COMMENT '段位',
  `FightCount` int(10) unsigned DEFAULT '0' COMMENT '匹配次数',
  `FightCountTime` int(10) unsigned DEFAULT '0' COMMENT '匹配次数恢复时间',
  `LoopTime` int(10) unsigned DEFAULT '0' COMMENT '上次循环操作时间',
  PRIMARY KEY (`PlayerID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家基础信息';

 CREATE TABLE IF NOT EXISTS `GPublicData` (
  `Key` varchar(20) NOT NULL COMMENT 'Key',
  `Value` varchar(100) DEFAULT '' COMMENT '值',
  PRIMARY KEY (`Key`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏公共数据';

DELIMITER /
CREATE PROCEDURE `PCreateRankTable` (IN RankTableName varchar(20))
BEGIN
    SET @sql=concat("CREATE TABLE IF NOT EXISTS `", RankTableName, "` (
      `Key` varchar(50) NOT NULL COMMENT 'Key',
      `Value` int(6) unsigned DEFAULT '0' COMMENT '值',
      `Rank` int(6) unsigned DEFAULT '0' COMMENT '排名',
      PRIMARY KEY (`Key`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='排行榜';");
    PREPARE exectable FROM @sql;
    EXECUTE exectable;
END;/
DELIMITER ;
