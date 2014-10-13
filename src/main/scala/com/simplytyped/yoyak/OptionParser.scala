package com.simplytyped.yoyak

import org.apache.commons.cli.{Options => Opts, HelpFormatter, CommandLine, PosixParser, OptionBuilder}

import scala.collection.mutable.ListBuffer

class OptionParser {
  val (opts,lineBuilder) : (Opts,List[(Options,CommandLine)=>Unit]) = {
    val optsBuffer = new Opts
    val lineBuildBuffer = new ListBuffer[(Options,CommandLine)=>Unit]

    /* add options here : START */
    OptionBuilder.withDescription("print help messages")
    optsBuffer.addOption(OptionBuilder.create("help"))
    lineBuildBuffer.append((o,c) => if(c.hasOption("help")) { printUsage(); System.exit(0) })

    OptionBuilder.withDescription("target apk to analyze")
    OptionBuilder.hasArg()
    OptionBuilder.withArgName("file")
    optsBuffer.addOption(OptionBuilder.create("target_apk"))
    lineBuildBuffer.append((o,c) => Option(c.getOptionValue("target_apk")).foreach{x => o.target_apk = List(x)})
    /* add options here : END */

    (optsBuffer,lineBuildBuffer.toList)
  }
  def generateOptions(cmdline : CommandLine) : Options = {
    val options = new Options
    lineBuilder.foreach{_(options,cmdline)}
    options
  }
  def printUsage() {
    val formatter = new HelpFormatter
    formatter.printHelp("yoyak",opts)
  }
  def parse(args: Array[String]) {
    val parser = new PosixParser
    val cmdline = parser.parse(opts,args)
    val options = generateOptions(cmdline)
    Options.g = options
  }
}
