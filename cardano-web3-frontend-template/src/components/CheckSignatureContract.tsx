import { AppStateContext, checkSignatureScript } from "@/pages/_app";
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
  UTxO,
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
    tokenSignatureNameHex,
    tokenSignaturePolicy,
    tokenSignatureAssetClassHex,
  } = appState;
  const [tokenName, setTokenName] = useState("");
  const [signature, setSignature] = useState("");
  const [amount, setAmount] = useState(10n);
  const [amountToLock, setValueToSend] = useState(15n);

  type GetFinalPolicy = {
    policyScript: MintingPolicy;
    unit: Unit;
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////////////
  const getFinalPolicy = async (): Promise<GetFinalPolicy> => {
    const tn = fromText(tokenSignatureNameHex!);
    const needPkh: string =
      getAddressDetails(signature).paymentCredential?.hash || "";
    const policyScript: MintingPolicy = {
      type: "PlutusV2",
      script: applyParamsToScript(
        "59064259063f01000032323232323232323232323232323232323232323232323232323232232223253355335353553335734603a6aae740044c8c8c8c848cc00400c008c04cd5d09aba200353335734603e6aae740044c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c848cccccccccccc00406005805004804003803002401c01400c008c0b8d5d09aba200233302475c40026ae84004d5d100119981101310009aba10013574400466042eb8d5d08009aba200353335734605e6aae740044c8c8c8c8c848cc0040100094ccd5cd18199aab9d00113232321233001003002301e357426ae88008cc079d69aba100135573c0020646ea8d5d09aba20035333573460626aae740044c8c8c848cc00400c008c070d5d09aba20023301c75a6ae84004d55cf0008181baa357420026aae780040b8dd51aba100135744004666036042eb4d5d08009aba20023301a01e357420026ae88008ccc05dd700b1aba10013574400466602aeb8050d5d08009aba200233014010357420026ae88008cc048034d5d08009aba20023301000b357420026aae78004078dd51aba100135573c002038034444444444444a66a666aa6605a4422444a66a00226a00644002442666a00a05a6008004666aa600e05200a008002044246600244a66a0044200620020504a666ae68cdc780880089812800812002101301180c899ab9c49011f42656e65666963696172792773207369676e6174757265206d697373696e670001b1120011637540026eb80048c94ccd5cd180d00080a8a999ab9a301900101301835573a6ea800488c8c94ccd5cd180e000889110008a999ab9a301b001132122230030043004357426aae7800854ccd5cd180d0008891100100c9aab9d0013754002464a666ae68c05cd55ce8008991919091980080180118029aba135744004601e6ae84004d55cf00080b1baa00123253335734602c6aae740044c8c8c8c8c8c8c8c8c8c848cccc00402401c00c008cc031d71aba135744008a666ae68c0800044c84888c008010d5d09aab9e002153335734603e002264244460020086eb8d5d09aab9e002153335734603c0022244400603a6aae74004dd51aba100135744004666012eb8020d5d08009aba20035333573460306aae740044c8c8c848cc00400c008cc01c03cd5d09aba2002300f357420026aae7800405cdd51aba100135573c00202a6ea800488c8c94ccd5cd180b8008980918021aba135573c0042a666ae68c060004044058d55ce8009baa0013300175ceb4888cc06088cccd55cf8009004119191919806091980080180118031aba2005300735573c004600e6aae74004d5d08010079bab00122330162233335573e002400c46600e600a6ae84008c00cd5d10010069bac001100c22122330010040032323253335734602600226424444600800a60086ae84d55cf0010a999ab9a30120011321222230020053005357426aae7800854ccd5cd180880089909111180080298039aba135573c0042a666ae68c0400044c848888c00c014dd71aba135573c00401e6aae74004dd500091919192999ab9a3370e90060010891111110018a999ab9a3370e90050010891111110020a999ab9a3370e90040010991909111111198008048041bad357426ae894008dd71aba1500115333573460260042646424444444660040120106eb8d5d09aba25002375c6ae85400454ccd5cd18090010991909111111198030048041bae357426ae894008c014d5d0a8008a999ab9a30110021321222222230070083005357426aae7800c54ccd5cd180800109909111111180280418029aba135573c00601e26aae78008d55ce8009baa0012323253335734601e0022646464646424466600200c0080066eb4d5d09aba2002375a6ae84004d5d10011bad357420026aae7800854ccd5cd1807000899091180100198021aba135573c00401a6aae74004dd5000919192999ab9a300e0011321223001003375c6ae84d55cf0010a999ab9a300d0011321223002003375c6ae84d55cf0010061aab9d0013754002464a666ae68c02cd55ce800899191909198008018011bad357426ae88008c010d5d08009aab9e00100a3754002464a666ae68c028d55ce80089bae357426aae78004024dd50008900088021091180100188800910010910008a99ab9c491035054310016370e90001b8748008dc3a40086e1d200623230010012233003300200200101",
        [needPkh],
      ),
    };
    const policyId: PolicyId = lucid!.utils.mintingPolicyToId(policyScript);
    const unit: Unit = policyId + tn;
    setAppState({
      ...appState,
      tokenSignatureIdHex: policyId,
      tokenSignatureAssetClassHex: unit,
      tokenSignaturePolicy: policyScript,
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

  const mintTx = async () => {
    console.log("mintTx -> appState: ", appState);
    const {
      policyScript: tokenSignaturePolicy,
      unit: tokenSignatureAssetClassHex,
    } = await getFinalPolicy();

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets(
        { [tokenSignatureAssetClassHex]: amount },
        Data.to(new Constr(0, [])),
      )
      .attachMintingPolicy(tokenSignaturePolicy)
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// BURN /////////////////////////////////////////////////////

  const burnTx = async () => {
    console.log("burnTx -> appState: ", appState);

    const unit = tokenSignatureAssetClassHex;
    const policyScript = tokenSignaturePolicy;

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
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// DEPLOY////////////////////////////////////////////////////

  const sentToContractTX = async () => {
    if (!lucid || !wAddr) {
      alert("Please connect Wallet");
      return;
    }

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || "";
    const needPkh: string =
      getAddressDetails(signature).paymentCredential?.hash || "";
    const validator = checkSignatureScript;

    if (!validator) {
      alert("Validator Script not defined!");
      return;
    }

    const validatorAddress = lucid!.utils.validatorToAddress(validator);

    const tx = await lucid!
      .newTx()
      .payToContract(
        validatorAddress,
        { inline: Data.to(needPkh, Data.Bytes) },
        { lovelace: amountToLock * 1000000n },
      )
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// CLAIM ////////////////////////////////////////////////////

  const claimContractTx = async () => {
    if (!lucid || !wAddr || !UTxOToClaim) {
      alert("Please connect Wallet");
      return;
    }

    const pkh: string = getAddressDetails(wAddr).paymentCredential?.hash || "";
    const validator = checkSignatureScript;

    if (!validator) {
      alert("Validator Script not defined!");
      return;
    }

    const tx = await lucid
      .newTx()
      .collectFrom([UTxOToClaim], Data.to(new Constr(0, [])))
      .attachSpendingValidator(validator)
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
          <p>Needed signature: (addr format)</p>
          <input
            className="py-1 px-2 ml-3 border border-zinc-700 rounded"
            type="string"
            onChange={(e) => {
              if (!e) return;
              setSignature(String(e.target.value));
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
            <button
              onClick={sentToContractTX}
              disabled={!lucid || !wAddr || !amountToLock || !signature}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Send to Contract
            </button>
          </div>
          <div className="w-full flex flex-row gap-4 mt-2">
            <p>UTxO Ref to claim:</p>

            <div className="flex flex-col mb-2">
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
              <div className="w-full flex flex-row gap-4 mt-2">
                <button
                  onClick={setUTxOToClaim}
                  disabled={!lucid || !wAddr || !UnlockUTxORef}
                  className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                  {" "}
                  Set UTxO to claim
                </button>

                <button
                  onClick={claimContractTx}
                  disabled={!lucid || !wAddr || !UTxOToClaim}
                  className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
                >
                  {" "}
                  Claim from Contract Tx
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
              value={tokenSignatureNameHex || ""}
              onChange={(e) => {
                const am = String(e.target.value);
                if (!am) return;
                setAppState({
                  ...appState,
                  tokenSignatureNameHex: am,
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
              onClick={mintTx}
              disabled={!lucid || !wAddr || !amount || !tokenSignatureNameHex}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Mint Tokens Tx
            </button>
            <button
              onClick={burnTx}
              disabled={
                !lucid ||
                !wAddr ||
                !amount ||
                !tokenSignatureNameHex ||
                !tokenSignaturePolicy
              }
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Burn Tokens Tx
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
