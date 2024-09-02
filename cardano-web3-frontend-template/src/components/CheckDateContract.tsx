import {
  AppStateContext,
  checkDateAfterScript,
  checkDateBeforeScript,
} from "@/pages/_app";
import {
  findUTxO,
  safeStringToBigInt,
  signAndSubmitTx,
} from "@/utilities/utilities";
import {
  applyParamsToScript,
  fromText,
  getAddressDetails,
  MintingPolicy,
  PolicyId,
  Unit,
} from "lucid-cardano";
import { Constr, Data } from "lucid-cardano";
import { useContext, useState } from "react";

export default function Stablecoin() {
  const { appState, setAppState } = useContext(AppStateContext);
  const {
    lucid,
    wAddr,
    contractClass,
    contractType,
    UTxOToClaim,
    UnlockUTxORef,
    tokenCheckDateNameHex,
    tokenCheckDateAfterPolicy,
    tokenCheckDateAfterAssetClassHex,

    tokenCheckDateBeforePolicy,
    tokenCheckDateBeforeAssetClassHex,
  } = appState;
  const [tokenName, setTokenName] = useState("");
  const [deadline, setDeadline] = useState(Number);
  const [amount, setAmount] = useState(10n);
  const [amountToLock, setValueToSend] = useState(15n);

  type GetFinalPolicy = {
    policyScript: MintingPolicy;
    unit: Unit;
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////////////
  const getPolicyCheckBeforeScript = async (): Promise<GetFinalPolicy> => {
    const tn = fromText(tokenCheckDateNameHex!);
    const policyScript: MintingPolicy = {
      type: "PlutusV2",
      script: applyParamsToScript(
        "59078259077f010000323232323232323232323232323232323232323232323232323232323232323232323232232223253355335353553335734604a6aae740044c8c8c8c848cc00400c008c04cd5d09aba200353335734604e6aae740044c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c848cccccccccccc00406005805004804003803002401c01400c008c0ccd5d09aba200233302475c40026ae84004d5d100119981101310009aba10013574400466042eb8d5d08009aba200353335734606e6aae740044c8c8c8cc0d14ccd5cd181d1aab9d002132330353301d75a6ae84004c070d5d09aba200135573c004072a666ae68c0e8d55ce8008991981a9980ebad3574200260386ae84d5d10009aab9e00103937546ae84d5d10011baa357420026aae780040d8dd51aba100135744004666036046eb4d5d08009aba20023301a01f357420026ae88008ccc05dd700b1aba10013574400466602aeb8050d5d08009aba200233014010357420026ae88008cc048034d5d08009aba20023301000b357420026aae78004098dd51aba100135573c00204844004444444444444a66a6a00a446a00444a666a66604604e60600280042a66a002266604204e006052266604204e006052056266604204e00605204e05a042266ae71240110446561646c696e652072656163686564000231120011637540026eb40048c94ccd5cd181100080e8a999ab9a302100101702035573a6ea800488c8c94ccd5cd181200080d8a999ab9a30230011301d3004357426aae7800854ccd5cd181100080d0109aab9d0013754002464a666ae68c07cd55ce8008991919091980080180118029aba13574400460266ae84004d55cf00080f1baa00123253335734603c6aae740044c8c8c8c8c8c8c8c8c8c848cccc00402401c00c008cc031d71aba135744008a666ae68c0a00044c84888c008010d5d09aab9e002153335734604e002264244460020086eb8d5d09aab9e002153335734604c00203a04a6aae74004dd51aba100135744004666012eb8020d5d08009aba20035333573460406aae740044c8c8c848cc00400c008cc01c044d5d09aba20023012357420026aae7800407cdd51aba100135573c00203a6ea800488c8c94ccd5cd180f800899091180100198021aba135573c0042a666ae68c080004054078d55ce8009baa0013300175ceb4888cc08088cccd55cf800900e119191919806091980080180118031aba2005300735573c004600e6aae74004d5d08010039bab001223301e2233335573e002403446600e600a6ae84008c00cd5d10010029bac0011200122122330010040032323253335734603600226424444600800a600a6ae84d55cf0010a999ab9a301a0011321222230020053007357426aae7800854ccd5cd180c80089909111180080298059aba135573c0042a666ae68c0600044c848888c00c014dd71aba135573c00402e6aae74004dd50009111a801111a801912999a9998048038020010a99a801880080b80880b91919192999ab9a3370e90060010891111110018a999ab9a3370e90050010891111110020a999ab9a3370e90040010991909111111198008048041bad357426ae894008dd71aba1500115333573460340042646424444444660040120106eb8d5d09aba25002375c6ae85400454ccd5cd180c8010991909111111198030048041bae357426ae894008c018d5d0a8008a999ab9a30180021321222222230070083006357426aae7800c54ccd5cd180b80109909111111180280418031aba135573c00602c26aae78008d55ce8009baa0012223253335003215333500321533350052130044984c00d261533350042130044984c00d2600f00f1533350042130034984c009261533350032130034984c0092600e153335002201000d00f15333500221533350042130034984c009261533350032130034984c0092600e00e1533350032130024984c005261533350022130024984c0052600d253335002215333500421533350042133300900700200116161600f15333500321533350032133300800600200116161600e00f2323253335734602a0022646464646424466600200c0080066eb4d5d09aba2002375a6ae84004d5d10011bad357420026aae7800854ccd5cd180a000899091180100198029aba135573c0040266aae74004dd500091a80091111111003919192999ab9a30130011321223001003375c6ae84d55cf0010a999ab9a30120011321223002003375c6ae84d55cf0010089aab9d0013754002246666666600244666ae68cdc3801000807007912999ab9a3370e0040020102a666ae68cdc480100080500491199ab9a3371000400201c01e44666ae68cdc480100080700791199ab9a3371200400201e01c44666ae68cdc4001000807807112999ab9a337120040022002200444a666ae68cdc4801000880108009192999ab9a300f35573a002264646424660020060046eb4d5d09aba20023005357420026aae78004038dd50009980309110008049192999ab9a300d35573a00226eb8d5d09aab9e00100c3754002201022444006224440042244400244246600200600442444600600820022440042440022a66ae712401035054310016370e90001b8748008dc3a40086e1d200623230010012233003300200200101",
        [BigInt(deadline)],
      ),
    };
    const policyId: PolicyId = lucid!.utils.mintingPolicyToId(policyScript);
    const unit: Unit = policyId + tn;
    setAppState({
      ...appState,
      tokenCheckDateBeforeIdHex: policyId,
      tokenCheckDateBeforeAssetClassHex: unit,
      tokenCheckDateBeforePolicy: policyScript,
    });

    return { policyScript, unit };
  };

  const getPolicyCheckAfterScript = async (): Promise<GetFinalPolicy> => {
    const tn = fromText(tokenCheckDateNameHex!);
    const policyScript: MintingPolicy = {
      type: "PlutusV2",
      script: applyParamsToScript(
        "59078159077e010000323232323232323232323232323232323232323232323232323232323232323232323232232223253355335353553335734604a6aae740044c8c8c8c848cc00400c008c04cd5d09aba200353335734604e6aae740044c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c848cccccccccccc00406005805004804003803002401c01400c008c0ccd5d09aba200233302475c40026ae84004d5d100119981101310009aba10013574400466042eb8d5d08009aba200353335734606e6aae740044c8c8c8cc0d14ccd5cd181d1aab9d002132330353301d75a6ae84004c070d5d09aba200135573c004072a666ae68c0e8d55ce8008991981a9980ebad3574200260386ae84d5d10009aab9e00103937546ae84d5d10011baa357420026aae780040d8dd51aba100135744004666036046eb4d5d08009aba20023301a01f357420026ae88008ccc05dd700b1aba10013574400466602aeb8050d5d08009aba200233014010357420026ae88008cc048034d5d08009aba20023301000b357420026aae78004098dd51aba100135573c002048440044444444444446a00a446a00444a666a66604604e60600280042a66a002266604204e006052266604204e006052056266604204e006052042266ae71240114446561646c696e65206e6f742072656163686564000231120011637540026eb40048c94ccd5cd181100080e8a999ab9a302100101702035573a6ea800488c8c94ccd5cd181200080d8a999ab9a30230011301d3004357426aae7800854ccd5cd181100080d0109aab9d0013754002464a666ae68c07cd55ce8008991919091980080180118029aba13574400460266ae84004d55cf00080f1baa00123253335734603c6aae740044c8c8c8c8c8c8c8c8c8c848cccc00402401c00c008cc031d71aba135744008a666ae68c0a00044c84888c008010d5d09aab9e002153335734604e002264244460020086eb8d5d09aab9e002153335734604c00203a04a6aae74004dd51aba100135744004666012eb8020d5d08009aba20035333573460406aae740044c8c8c848cc00400c008cc01c044d5d09aba20023012357420026aae7800407cdd51aba100135573c00203a6ea800488c8c94ccd5cd180f800899091180100198021aba135573c0042a666ae68c080004054078d55ce8009baa0013300175ceb4888cc08088cccd55cf800900e119191919806091980080180118031aba2005300735573c004600e6aae74004d5d08010039bab001223301e2233335573e002403446600e600a6ae84008c00cd5d10010029bac0011200122122330010040032323253335734603600226424444600800a600a6ae84d55cf0010a999ab9a301a0011321222230020053007357426aae7800854ccd5cd180c80089909111180080298059aba135573c0042a666ae68c0600044c848888c00c014dd71aba135573c00402e6aae74004dd50009111a801111a801912999a9998048038020010a99a801880080b80880b91919192999ab9a3370e90060010891111110018a999ab9a3370e90050010891111110020a999ab9a3370e90040010991909111111198008048041bad357426ae894008dd71aba1500115333573460340042646424444444660040120106eb8d5d09aba25002375c6ae85400454ccd5cd180c8010991909111111198030048041bae357426ae894008c018d5d0a8008a999ab9a30180021321222222230070083006357426aae7800c54ccd5cd180b80109909111111180280418031aba135573c00602c26aae78008d55ce8009baa0012223253335003215333500321533350052130044984c00d261533350042130044984c00d2600f00f1533350042130034984c009261533350032130034984c0092600e153335002201000d00f15333500221533350042130034984c009261533350032130034984c0092600e00e1533350032130024984c005261533350022130024984c0052600d253335002215333500421533350042133300900700200116161600f15333500321533350032133300800600200116161600e00f2323253335734602a0022646464646424466600200c0080066eb4d5d09aba2002375a6ae84004d5d10011bad357420026aae7800854ccd5cd180a000899091180100198029aba135573c0040266aae74004dd500091a80091111111003919192999ab9a30130011321223001003375c6ae84d55cf0010a999ab9a30120011321223002003375c6ae84d55cf0010089aab9d0013754002246666666600244666ae68cdc3801000807007912999ab9a3370e0040020102a666ae68cdc480100080500491199ab9a3371000400201c01e44666ae68cdc480100080700791199ab9a3371200400201e01c44666ae68cdc4001000807807112999ab9a337120040022002200444a666ae68cdc4801000880108009192999ab9a300f35573a002264646424660020060046eb4d5d09aba20023005357420026aae78004038dd50009980309110008049192999ab9a300d35573a00226eb8d5d09aab9e00100c3754002201022444006224440042244400244246600200600442444600600820022440042440022a66ae712401035054310016370e90001b8748008dc3a40086e1d200623230010012233003300200200101",
        [BigInt(deadline)],
      ),
    };
    const policyId: PolicyId = lucid!.utils.mintingPolicyToId(policyScript);
    const unit: Unit = policyId + tn;
    setAppState({
      ...appState,
      tokenCheckDateAfterIdHex: policyId,
      tokenCheckDateAfterAssetClassHex: unit,
      tokenCheckDateAfterPolicy: policyScript,
    });

    return { policyScript, unit };
  };

  const setUTxOToClaim = async () => {
    if (!lucid || !UnlockUTxORef) {
      return;
    }
    const UTxOToUnlock = await findUTxO(lucid, UnlockUTxORef);
    console.log("Set UTxO to claim: ", UTxOToUnlock);
    setAppState({
      ...appState,
      UTxOToClaim: UTxOToUnlock,
    });
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// MINT /////////////////////////////////////////////////////

  const mintAfterContractTx = async () => {
    console.log("mintTx -> appState: ", appState);
    const {
      policyScript: tokenCheckDateAfterPolicy,
      unit: tokenCheckDateAfterAssetClassHex,
    } = await getPolicyCheckAfterScript();

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets(
        { [tokenCheckDateAfterAssetClassHex]: amount },
        Data.to(new Constr(0, [])),
      )
      .attachMintingPolicy(tokenCheckDateAfterPolicy)
      .validFrom(Number(deadline))
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  const mintBeforeContractTx = async () => {
    console.log("mintTx -> appState: ", appState);
    const {
      policyScript: tokenCheckDateBeforePolicy,
      unit: tokenCheckDateBeforeAssetClassHex,
    } = await getPolicyCheckBeforeScript();

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets(
        { [tokenCheckDateBeforeAssetClassHex]: amount },
        Data.to(new Constr(0, [])),
      )
      .attachMintingPolicy(tokenCheckDateBeforePolicy)
      .validTo(Number(deadline))
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// BURN /////////////////////////////////////////////////////

  const burnAfterContractTx = async () => {
    console.log("burnTx -> appState: ", appState);

    const unit = tokenCheckDateAfterAssetClassHex;
    const policyScript = tokenCheckDateAfterPolicy;

    if (!unit || !policyScript) {
      console.log("NFT script not found");
      return;
    }

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets({ [unit]: -amount }, Data.to(new Constr(0, [])))
      .attachMintingPolicy(policyScript)
      .validFrom(Number(deadline))
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  const burnBeforeContractTx = async () => {
    console.log("burnTx -> appState: ", appState);

    const unit = tokenCheckDateBeforeAssetClassHex;
    const policyScript = tokenCheckDateBeforePolicy;

    if (!unit || !policyScript) {
      console.log("NFT script not found");
      return;
    }

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets({ [unit]: -amount }, Data.to(new Constr(0, [])))
      .attachMintingPolicy(policyScript)
      .validTo(Number(deadline))
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// DEPLOY////////////////////////////////////////////////////

  const deployAfterContractTx = async () => {
    if (!lucid || !wAddr) {
      alert("Please connect Wallet");
      return;
    }

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || "";
    const validator = checkDateAfterScript;

    if (!validator) {
      alert("Validator Script not defined!");
      return;
    }

    const validatorAddress = lucid!.utils.validatorToAddress(validator);

    const tx = await lucid!
      .newTx()
      .payToContract(
        validatorAddress,
        { inline: Data.to(BigInt(deadline), Data.Integer()) },
        { lovelace: amountToLock * 1000000n },
      )
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  const deployBeforeContractTx = async () => {
    if (!lucid || !wAddr) {
      alert("Please connect Wallet");
      return;
    }

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || "";
    const validator = checkDateBeforeScript;

    if (!validator) {
      alert("Validator Script not defined!");
      return;
    }

    const validatorAddress = lucid!.utils.validatorToAddress(validator);

    const tx = await lucid!
      .newTx()
      .payToContract(
        validatorAddress,
        { inline: Data.to(BigInt(deadline), Data.Integer()) },
        { lovelace: amountToLock * 1000000n },
      )
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// CLAIM ////////////////////////////////////////////////////

  const claimAfterContractTx = async () => {
    if (!lucid || !wAddr || !UTxOToClaim) {
      alert("Please connect Wallet");
      return;
    }

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || "";
    const validator = checkDateAfterScript;

    if (!validator) {
      alert("Validator Script not defined!");
      return;
    }

    const tx = await lucid
      .newTx()
      .collectFrom([UTxOToClaim], Data.to(new Constr(0, [])))
      .attachSpendingValidator(validator)
      .validFrom(Number(Data.from(UTxOToClaim.datum!, Data.Integer())))
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  const claimBeforeContractTx = async () => {
    if (!lucid || !wAddr || !UTxOToClaim) {
      alert("Please connect Wallet");
      return;
    }

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || "";
    const validator = checkDateBeforeScript;

    if (!validator) {
      alert("Validator Script not defined!");
      return;
    }
    console.log(Data.from(UTxOToClaim.datum!, Data.Integer()));
    const tx = await lucid
      .newTx()
      .collectFrom([UTxOToClaim], Data.to(new Constr(0, [])))
      .attachSpendingValidator(validator)
      .validTo(Number(Data.from(UTxOToClaim.datum!, Data.Integer())))
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };
  ///////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////// UI /////////////////////////////////////////////////

  return (
    <div className="text-zinc-800 font-quicksand">
      <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9">
        <div className="w-full flex flex-row gap-4 mt-2">
          <p>Set deadline:</p>
          <input
            className="py-1 px-2 ml-3 border border-zinc-700 rounded"
            type="datetime-local"
            // value={deadline}
            onChange={(e) => {
              const selectedDate = e.target.value;
              const timestamp = new Date(selectedDate).getTime();
              console.log(selectedDate, "    ", timestamp);
              if (!timestamp) return;
              setDeadline(timestamp);
            }}
          />
        </div>
      </div>
      {contractType == "validator" && (
        <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9">
          <div className="w-full flex flex-row gap-4 mt-2">
            <p>Amount (in ADA):</p>
            <input
              className="w-16 py-1 px-2 ml-3 border border-zinc-700 rounded"
              type="number"
              value={Number(amountToLock)}
              onChange={(e) => {
                const coll = safeStringToBigInt(e.target.value);
                if (!coll) return;
                setValueToSend(coll);
              }}
            />
          </div>

          <div className="w-full flex flex-row justify-center gap-4 mt-2">
            <button
              onClick={deployBeforeContractTx}
              disabled={!lucid || !wAddr || !amountToLock || !deadline}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Send to Before Contract Tx
            </button>
            <button
              onClick={deployAfterContractTx}
              disabled={!lucid || !wAddr || !amountToLock || !deadline}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Send to After Contract Tx
            </button>
          </div>
          <div className="w-full flex flex-row gap-4 mt-2">
            <div className="flex flex-col mb-2">
              <div className="w-full flex flex-row gap-4 mt-2">
                <p>UTxO Ref to claim:</p>
                <input
                  className="py-1 px-2 border border-zinc-700 rounded"
                  type="string"
                  value={UnlockUTxORef || ""}
                  onChange={(e) =>
                    setAppState({
                      ...appState,
                      UnlockUTxORef: e.target.value,
                      UTxOToClaim: undefined,
                    })
                  }
                />
                <button
                  onClick={setUTxOToClaim}
                  disabled={!lucid || !wAddr || !UnlockUTxORef}
                  className="rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                  {" "}
                  Set UTxO to claim
                </button>
              </div>
              <div className="w-full flex flex-row gap-4 mt-2">
                <button
                  onClick={claimBeforeContractTx}
                  disabled={!lucid || !wAddr || !UTxOToClaim}
                  className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                  {" "}
                  Claim from Before Contract Tx
                </button>
                <button
                  onClick={claimAfterContractTx}
                  disabled={!lucid || !wAddr || !UTxOToClaim}
                  className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                  {" "}
                  Claim from After Contract Tx
                </button>
              </div>
            </div>
          </div>
        </div>
      )}
      {contractType == "policy" && (
        <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9">
          <div className="w-full flex flex-row gap-4 mt-2">
            <p>Token name:</p>
            <input
              className="w-160 py-1 px-2 ml-2 border border-zinc-700 rounded"
              type="string"
              value={tokenCheckDateNameHex || ""}
              onChange={(e) => {
                const am = String(e.target.value);
                if (!am) return;
                setAppState({
                  ...appState,
                  tokenCheckDateNameHex: am,
                });
              }}
            />
            <p>Token amount (units):</p>
            <input
              className="w-16 py-1 px-2 ml-2 border border-zinc-700 rounded"
              type="number"
              value={Number(amount)}
              onChange={(e) => {
                const am = safeStringToBigInt(e.target.value);
                if (!am) return;
                setAmount(am);
              }}
            />
          </div>
          <div className="w-full flex flex-row justify-center gap-4 mt-2">
            <button
              onClick={mintBeforeContractTx}
              disabled={!lucid || !wAddr || !amount || !tokenCheckDateNameHex}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Mint Tokens using Before Contract Tx
            </button>
            <button
              onClick={burnBeforeContractTx}
              disabled={
                !lucid ||
                !wAddr ||
                !amount ||
                !tokenCheckDateNameHex ||
                !tokenCheckDateBeforePolicy
              }
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Burn Tokens using Before Contract Tx
            </button>
          </div>
          <div className="w-full flex flex-row justify-center gap-4 mt-2">
            <button
              onClick={mintAfterContractTx}
              disabled={!lucid || !wAddr || !amount || !tokenCheckDateNameHex}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Mint Tokens using After Contract Tx
            </button>
            <button
              onClick={burnAfterContractTx}
              disabled={
                !lucid ||
                !wAddr ||
                !amount ||
                !tokenCheckDateNameHex ||
                !tokenCheckDateAfterPolicy
              }
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Burn Tokens using After Contract Tx
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
